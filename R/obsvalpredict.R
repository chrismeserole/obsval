obsvalPredict <- function( model, ... , 
                           subsample.var=NULL, subsample.val=NULL,
                           col.names=NULL, verbose=FALSE, ci=95 ){
  
  if(verbose==T) cat('Now in obsvalPredict() ... \n')

  args <- list(...)
  
  if('reg.model' %in% names(args)){
    reg.model <- args[['reg.model']]
  } else {
    message("obsvalPredict() must be passed a 'reg.model' parameter.")
    stop()
  }
  
  if('sim.coefs' %in% names(args)){
    sim.coefs <- args[['sim.coefs']]
  } else {
    message("obsvalPredict() must be passed a 'sim.coefs' parameter.")
    stop()
  }
  
  if('effect.var' %in% names(args)){
    effect.var <- args[['effect.var']]
  } else {
    message("obsvalPredict() must be passed an 'effect.var' parameter.")
    stop()
  }
  
  if('effect.vals' %in% names(args)){
    effect.vals <- args[['effect.vals']]
  } else {
    message("obsvalPredict() must be passed an 'effect.vals' parameter.")
    stop()
  }
  
  
  #
  # SET VARS -----------------------------------------------------------------
  # 
  n.dv.cat <- getNumDVCategories( model, reg.model )
  low.ci.val <- ((100-as.numeric(ci))/2)/100
  high.ci.val <- 1-low.ci.val
  
  # set n.draws
  if(length(dim(sim.coefs)) == 2 ) {
    n.draws <- length(sim.coefs[,1])
  }
  else {
    n.draws <- length(sim.coefs[,1,1])
  }
  
  
  #
  # CLEAN DATA ----------------------------------------------------------------
  #
  confirmEffectVar(effect.var, as.formula(model))

  # ensure there's model.frame() object
  # TODO: re-run model if absent?
  if(!("data" %in% names(model))){
    if("model" %in% names(model)){ #this inner loop obsolete for clusterSEs
      model$data <- model$model
    }
    else{
      message('obsvalPredict() expects a model.frame() object stored in 
               model$data or model$model.')
      stop()  
    }
  }
  
  # generate cleaned factor object:
  # 1. checks that all specified vals are actually 
  # in the factor vector
  # 2. converts both vector and specified values to numeric
  obj <- cleanFactorVar(model$data[[effect.var]],
                        effect.vals,
                        "effect var",
                        verbose)
  model$data[[effect.var]] <- obj$vec
  effect.vals <- obj$vals
  
  
  # check subsample.var, must be before matrix
  # note: only relevant if obsvalPredict() called outside obsval()
  if( !is.null(subsample.var) ){
    if( is.factor(subsample.var)){
      message("The subsample var cannot be a factor. Try splitting into numeric dummies.")
      stop(options(show.error.messages=FALSE))
    }
  }
  
  
  #
  # BUILD X.matrix ------------------------------------------------------------
  #
  
  if(verbose==TRUE){
    cat('Constructing X.matrix ... ')
  }  

  # get the data that was actually used to create the model
  # note: model.matrix() automatically prepends a column of '1' for constant
  if(!("matrix" %in% names(model))){
    X.matrix <- model.matrix(model, model$data)
  }
  else {
    X.matrix <- model$matrix
  }
  
  # subset the data frame so that only cases matching the specifed
  # sub sample value are included in X.matrix
  if(!is.null(subsample.var)){
      X.matrix <- getSubset( X.matrix, subsample.var, subsample.val )
      n.obs <- length(X.matrix[, 1])
  } 

  # Get number of observations
  n.obs <- length(X.matrix[, 1])  

  
  #
  # GEN CONTROL/BASELINE PREDICTIONS ------------------------------------------
  # 
  if(verbose==TRUE){
    cat('Generating control predictions ... \n')
  }  

  control.preds <- computePreds( model, reg.model,
                             sim.coefs, effect.var, "no_manipulation",
                             X.matrix, col.names, verbose )
  
  
  #
  # SET UP EFFECT VAR VALUES --------------------------------------------------
  # 
  n.effect.vals <- length(effect.vals)
  effect.names <- rep(NA, n.effect.vals)  
  
  
  #
  # INITIALIZE DATA STORES  --------------------------------------------------
  # 
  if( reg.model=="ols" || reg.model=='logit' || reg.model=='clogit' || 
        reg.model=='probit' || reg.model=='poisson' || reg.model=='negbin' ){      
    preds <- matrix(NA,nrow=n.draws, ncol=n.effect.vals)    
  } else {
    preds <- array(NA,dim=(c(n.draws, n.dv.cat,  n.effect.vals)))
  }
  

  #
  # GENERATE PREDICTIONS FOR EFFECT VAR VALUES --------------------------------
  #
  if(verbose==TRUE) {
    cat("Generating predictions for each set of simulated coefficients ...\n")
  }
  for(i in 1:n.effect.vals){
    
    if( length(dim(preds))==2 ){
      preds[,i] <- computePreds( model, reg.model,  
                             sim.coefs, effect.var, effect.vals[i],
                              X.matrix, col.names )
    } else {
      
      preds[,,i] <- computePreds( model, reg.model, 
                              sim.coefs, effect.var, effect.vals[i],
                              X.matrix, col.names )
      
    }
    
    effect.names[i] <- concat("var_", effect.vals[i])
    
  }
  
  # set for multidimensional preds (mlogit)
  if(length(dim(preds)) > 2){
    dimnames(preds)[[2]] <- col.names
    dimnames(preds)[[3]] <- effect.names
  }
  

  if(verbose==TRUE)
  {
    cat("Calculated predictions for each set of simulated coefficients.\n")
  }
  
  
  
  
  # 
  # COMPUTE CONTROL OBJECTS -----------------------------------------------
  #
  
  if( is.null(dim(control.preds)) ){
    
    control.mean <- mean(control.preds)
    control.low.ci <- quantile(control.preds,low.ci.val)
    control.high.ci <-quantile(control.preds,high.ci.val)
    
  } else {
    
    control.mean=getMeansAsMatrix(control.preds, n.dv.cat)
    control.low.ci=getCIsAsMatrix(control.preds, low.ci.val, n.dv.cat)
    control.high.ci=getCIsAsMatrix(control.preds, high.ci.val, n.dv.cat)
    
    colnames(control.mean) <- colnames(control.preds)
    colnames(control.low.ci) <- colnames(control.preds)
    colnames(control.high.ci) <- colnames(control.preds)
    
    dvcatnames <- colnames(control.preds)
    
  }
  
  effect.var.low <- effect.vals[1]
  effect.var.high <- effect.vals[length(effect.vals)]
  
  # store index positions to calculate effect.preds
  for(i in 1:n.effect.vals){
    if( effect.vals[i] == effect.var.low ){
      low.index.pos <- i
    }
    else if( effect.vals[i] == effect.var.high ){
      high.index.pos <- i
    }
  }
  
  
  
  # set names for the preds matrix/array
  if( length(dim(preds))==2){
    
    # the getMeans() functions are wrappers for apply()
    means=getMeansAsMatrix(preds, n.effect.vals) 
    low.ci=getCIsAsMatrix(preds,low.ci.val, n.effect.vals)
    high.ci=getCIsAsMatrix(preds,high.ci.val, n.effect.vals)
    
    effect.preds <- preds[, high.index.pos]-preds[, low.index.pos]
    
    effect.mean <- mean(effect.preds)
    effect.low.ci <- quantile(effect.preds, low.ci.val)
    effect.high.ci <- quantile(effect.preds, high.ci.val)
    
    colnames(preds) <- effect.names
    colnames(means) <- effect.names
    colnames(low.ci) <- effect.names
    colnames(high.ci) <- effect.names
    
    effect_sum <- effectSum( effect.low.ci, effect.mean, effect.high.ci,
                             low.ci.val, high.ci.val )
    
  } 
  else {    
    
    means=matrix(NA,nrow=n.effect.vals,ncol=n.dv.cat)
    low.ci=matrix(NA,nrow=n.effect.vals,ncol=n.dv.cat)
    high.ci=matrix(NA,nrow=n.effect.vals,ncol=n.dv.cat)
    
    for( i in 1:n.effect.vals ){
      
      means[i, ] <- getMeansAsMatrix(preds[, , i], n.dv.cat)
      low.ci[i, ] <- getCIsAsMatrix(preds[, , i], low.ci.val, n.dv.cat)      
      high.ci[i, ] <- getCIsAsMatrix(preds[, , i], high.ci.val, n.dv.cat)
      
    }
    
    effect.preds <- preds[,,high.index.pos]-preds[,,low.index.pos]
    
    effect.mean=getMeansAsMatrix(effect.preds, n.dv.cat) 
    effect.low.ci=getCIsAsMatrix(effect.preds, low.ci.val, n.dv.cat)
    effect.high.ci=getCIsAsMatrix(effect.preds, high.ci.val, n.dv.cat)
    
    colnames(means) <- dvcatnames
    colnames(low.ci) <- dvcatnames
    colnames(high.ci) <- dvcatnames
    
    rownames(means) <- effect.names
    rownames(low.ci) <- effect.names
    rownames(high.ci) <- effect.names    
    
    colnames(effect.preds) <- dvcatnames
    colnames(effect.mean) <- dvcatnames
    colnames(effect.low.ci) <- dvcatnames
    colnames(effect.high.ci) <- dvcatnames
    
    # effectSum() must run after colname assignment, for use as rownames
    effect_sum <- effectSum( effect.low.ci, effect.mean, effect.high.ci,
                             low.ci.val, high.ci.val )    
    
  }  
    
  output <- list(model=model,
                 sim.coefs=sim.coefs,
                 preds=preds,
                 means=means,
                 low.ci=low.ci,
                 high.ci=high.ci,
                 control.preds=control.preds,
                 control.mean=control.mean,
                 control.low.ci=control.low.ci,
                 control.high.ci=control.high.ci,
                 effect.preds=effect.preds,
                 effect.mean=effect.mean,
                 effect.low.ci=effect.low.ci,
                 effect.high.ci=effect.high.ci,
                 effect_sum=effect_sum,
                 effect.var=effect.var,
                 reg.model=reg.model)
  
  return(output)
  
}