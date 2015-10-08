#' Obsval: Using Observed Values to Calculate Predicted Effects
#'
#' This function calculates predicted probabilities, discrete differences and marginal effects using the observed values of covariates rather than holding covariates constant at their means. It is based on Hanmer and Kalkan (2013).
#' @param data Data frame containing variables specified by effect.var and fmla.
#' @param fmla A string or formula containing the model specification. Should be of the form 'x ~ y'.
#' @param reg.model A string containing the type of regression to run. Available options are ols, logit, ologit, mlogit, probit, oprobit, poisson, negbin and mlogit.
#' @param n.draws An integer specifying how many draws of coefficient estimates to make. Defaults to 1000.
#' @param effect.var A string containing the name of the variable whose values will be specified in effect.var.low and effect.var.high
#' @param effect.vals An integer or vector specifying the values of effect.var to use. Values can be numeric or character. Effect.preds are calculated by substracting the predictions for final value of effect.vals from the predictions for the initial value. 
#' @param cluster.se A string specifying the variable name to use as the cluster variable when computing cluster-robust standard errors.
#' @param subsample.var A string specifying the variable to use to subset the data post-estimation.
#' @param subsample.val A string or integer specifying the value to use to subset the data post-estimation. If subsample.var is "Sex" and subsample.val is "Male", then only observations in which the "Sex" variable is set to "Male" will be used to compute effects.  
#' @param baseline.category A string specifying the category of the dependent variable to use as a baseline when estimating multinomial models.
#' @param seed An integer specifying the seed to use when computing simulations. Defaults to 123.
#' @param ci An integer specifying the confidence interval to use when returning effect summary tables. Defaults to 95.
#' @param verbose A boolean specifying whether to print messages when running obsval(). Useful for debugging. Defaults to FALSE. intervoptions A list of optional variables, including verbose, baseline.category, seed and sample_var.
#' @export
#' @examples
#' 
#' # run the model + get estimates
#' mymodel <- obsval(binary_warsup~female+partyid,
#'                    data = studentVote, 
#'                    reg.model = "logit",
#'                    n.draws = 100,
#'                    effect.var = "female", 
#'                    effect.vals = c(0,1),
#'                    verbose = TRUE)
#'                   
#' # get mean and quantile estimates
#' mean(mymodel$effect_preds)
#' quantile(mymodel$effect_preds, c(0.025,0.975))
#' 
#' # visualize estimated effect on probability
#' #boxplot(mymodel$effect_preds)

obsval <- function( fmla, data, reg.model=NULL, n.draws=1000,
                    effect.var=NULL, effect.vals=NULL,
                    cluster.se=NULL, 
                    subsample.var=NULL, 
                    subsample.val=NULL,
                    baseline.category=NULL,
                    verbose=FALSE, seed=123, ci=95){
  
  
  if(typeof(fmla)=='character'){
    fmla <- as.formula(fmla)
  } 
  if(typeof(fmla) != 'language'){
    message("The 1st argument passed to obsval() must be a formula or string.") 
    stop()
  }
  
  if(is.null(data)){
    message("obsval() requires a data object.") 
    stop()
  }
  
  if(is.null(reg.model)){
    message("obsvalPredict() must be passed a 'reg.model' parameter.")
    stop()
  }


  
	##
	##	CHECK ARGUMENTS + DATA  ---------------------------------------------------
	## 
  # get dependent variable
	dep_var 	  	<- all.vars(fmla)[1]

  # clean and reset dependent var
  data[[dep_var]] <- cleanDV(data[[dep_var]], reg.model, verbose)
 
  # generate cleaned effect.var object
  if(!is.null(effect.var)){
    confirmEffectVar(effect.var, fmla)
    obj <- cleanFactorVar(data[[effect.var]], 
                          effect.vals,
                          "effect var",
                          verbose)
    data[[effect.var]] <- obj$vec
    effect.vals <- obj$vals
  }
  
  # generate cleaned subsample.var object
  if(!is.null(subsample.var)){
    confirmEffectVar(subsample.var, fmla)
    obj <- cleanFactorVar(data[[subsample.var]], 
                          subsample.val,
                          "sub sample var",
                          verbose)
    data[[subsample.var]] <- obj$vec
    subsample.val <- obj$vals
  }
  
  # initialize col.names, which not all regression models set. since 
  # we're passing it to obsvalPredict(), we need to set it here
  col.names = NULL
  
  
	#
	#	ESTIMATE MODEL -------------------------------------------------------------
	#

	if(verbose==TRUE) cat("Estimating model...\n")
	
  if(reg.model=="ols"){
    model <- lm(fmla, data=data)
  }
	else if(reg.model=="logit"){
    model <- glm(fmla, family=binomial(link="logit"),data=data)	
	}  
  else if(reg.model=="clogit"){
    if (!requireNamespace("survival", quietly = TRUE)) {
      stop("The package \"survival\" is needed to run a clogit regression.",
           call. = FALSE)
    }
    require("survival", quietly = TRUE)
	  model <- survival::clogit(fmla, data=data, model=TRUE)	
	}
  else if(reg.model=="probit"){
		model <- glm(fmla, family=binomial(link="probit"),data=data)	
	} else if(reg.model=="negbin"){
		model <- MASS::glm.nb(fmla,data=data)	
	} else if(reg.model=="poisson"){
		model <- glm(fmla, family=poisson(link="log"),data=data)
	} else if(reg.model=="ologit"){
		model <- MASS::polr(formula=fmla, data=data, 
                        method="logistic", Hess=TRUE)
		# polr separates coefficients and intercepts, but to draw from posterior 
    # distribution using rmvnorm, we need to create a single vector
		model$coef 		<- append(model$coef, model$zeta)
		n.cuts 			<- length(model$zeta)
		n.coefs 		<- length(model$coefficients)
		col.names 		<- levels(data[[dep_var]])
	} else if(reg.model=="oprobit"){
		model 			<- MASS::polr(formula=fmla, data=data, 
                           method="probit", Hess=TRUE)
		model$coef 		<- append(model$coef, model$zeta)
		n.cuts 			<- length(model$zeta)
		n.coefs 		<- length(model$coefficients)
		col.names 		<- levels(data[[dep_var]])
	} else if(reg.model=="mlogit"){
	  if (!requireNamespace("nnet", quietly = TRUE)) {
	    stop("The package \"nnet\" is needed to run a clogit regression.",
	         call. = FALSE)
	  }
	  require("nnet", quietly = TRUE)
    
    # multinom objects does not store model.frame(), and worse, compute it 
    # in separate environment. code below corrects for this.
	  environment(fmla)<-environment()
	  confirmBaselineCategory(baseline.category)    
    data[[dep_var]]	<- relevel(data[[dep_var]], ref=baseline.category)	  
    model 			<- multinom(formula=fmla, data=data, Hess=TRUE)		
    model$vcov <- solve(model$Hessian)
    model$data <- model.frame(model, data)
    model$matrix <- model.matrix(model, model$data)
    col.names		<-cbind(c(rownames(coef(model)), baseline.category))
	}
	else {
    s <- 'Supported reg.model types include only \"logit\", \"clogit\", '
    s <- paste(s, '\"ologit\", \"mlogit\", \"probit\", \"oprobit\",')
    s <- paste(s, '\"negbin\", or \"poisson\".\n')
		message(s)
		stop(options(show.error.messages=FALSE))
	} 

	if(verbose==TRUE) {
		if(exists("model")){
			cat("Done estimating model.\n")
		}
		else{
			message("The model was not able to be estimated.\n")
			stop()
		}
	}
  
  ##
  ##  COMPUTE CLUSTERED STANDARD ERRORS
  ##
  if(!is.null(cluster.se)){
    if(!reg.model == "mlogit"){
      model$data <- data[,c(all.vars(as.formula(model)),cluster.se)]
      model$data <- model$data[complete.cases(model$data),]
      model$vcov <- cl(model$data,model,model$data[[cluster.se]])
    } else {
      message("Clustered standard errors are not yet supported for mlogit.")
      stop()
    }
  }
  
  
  ##
  ##  RETURN EARLY IF NOT CALCULATING EFFECTS
  ##
  if(is.null(effect.var)){
    return(model)
  }
  

	##
	## GENERATE SIMULATED COEFFICIENTS ------------------------------------------
	##
	
  # generate vector, matrix or array of simulated coefficients
  sim.coefs <- drawSimCoefs( model = model, 
                             reg.model = reg.model, 
                             n.draws=n.draws, 
                             verbose=verbose,
                             seed=seed ) 
  
  
  ##
  ## GENERATE PREDICTIONS  -----------------------------------------------------
  ##
  
  obsval_object <- obsvalPredict( model = model,
                                  reg.model = reg.model, 
                                  sim.coefs = sim.coefs, 
                                  effect.var = effect.var, 
                                  effect.vals = effect.vals,
                                  col.names = col.names,
                                  verbose = verbose,
                                  subsample.var = subsample.var,
                                  subsample.val = subsample.val
                                  )
	
  
  ##
  ## RETURN OBJECT        -------------------------------------------------------
  ##
  
	return(obsval_object)
}