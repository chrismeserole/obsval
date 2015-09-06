#' Generate Simulated Coefficients
#'
#' This function generates simulated coefficients based on coefficient means and covariances.
#' @param model A model object. For ordered regression, make sure the cutpoints are appended to model$coef.
#' @param reg.model A string containing the type of regression to run. Available options are logit, clogit, ologit, mlogit, probit, oprobit, poisson, negbin.
#' @param n.draws An integer specifying how many draws of coefficient estimates to make. 
#' @param options A list of optional variables, including verbose and seed.
#' @export
#' @examples
#'
#' library(arm)
#' library(mvtnorm)
#'
#' n.draws <- 100 
#' reg.model <- "logit"
#' 
#' model <- glm('binary_warsup~female+partyid', family=binomial(link="logit"),
#'                data=studentVote)  
#' 
#' sim.coefs <- drawSimCoefs(model, reg.model, n.draws)
#' 
#' head(sim.coefs)
#' 

drawSimCoefs <- function(model, reg.model, n.draws = 1000, 
                          verbose = FALSE, seed = 123){

  if (!requireNamespace("mvtnorm", quietly = TRUE)) {
    stop("The package \"mvtnorm\" is needed for drawSimCoefs to work.",
         call. = FALSE)
  }
  
  
  set.seed(seed)
  
  if(verbose==TRUE){
    cat("Drawing simulated coefficients from posterior distribution...\n")
  }

  if(reg.model != 'mlogit'){
    if("vcov" %in% names(model)){
      # this will be true if clustered standard errors set
      sim.coefs <- mvtnorm::rmvnorm(n.draws, model$coef, model$vcov)  
    } else {
      sim.coefs <- mvtnorm::rmvnorm(n.draws, model$coef, vcov(model))  
    }
  }
  else if(reg.model == 'mlogit'){
    
    # define variables we need to draw from posterior distribution
    n.coefs <- dim(coef(model))[2]
    n.coef.sets <- dim(coef(model))[1]
    
    # create coef matrix we'll use to hold draws from posterior distribution
    # we need matrix in this case b/c we have a separate set of coefficients
    # for each non-baseline level of the dependent variable
    sim.coefs <- array(NA, dim=(c(n.draws, n.coefs, n.coef.sets)))
    
    # draw from posterior distribution
    for(j in 1:n.coef.sets){
      
      # multinom() doesn't return separate vcov() matrices for each non-baseline
      # value of dependent variable. instead it returns one large vcov() matrix, 
      # so we have to splice it into separate vcov() matrices
      dim1 <- (j - 1) * n.coefs + 1
      dim2 <- j * n.coefs
 
      # now that we've got separate vcov() matrices, we can draw from 
      # multivariate distribution for each set of coefficients
      # note: for some reason rstudio was throwing an error on vcov(model)
      # when passed to this function, so obsval() now computes vcov directly
      # vcov(model) still provided for direct calls to obsvalPredict, which 
      # estimate model outside obsval() and won't have model$vcov object
      if('vcov' %in% names(model)){
        sim.coefs[, , j] <- mvtnorm::rmvnorm(n.draws, 
                                           coef(model)[j, ], 
                                           model$vcov[dim1:dim2, dim1:dim2]) 
      } 
      else {
        sim.coefs[, , j] <- mvtnorm::rmvnorm(n.draws, 
                                           coef(model)[j, ], 
                                           vcov(model)[dim1:dim2, dim1:dim2]) 
      }
    }
  }
  
  if(verbose==TRUE){
    cat("Finished drawing simulated coefficients from posterior distribution...\n")
  }
  return(sim.coefs)
}
