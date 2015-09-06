#' Graph Effect Range
#'
#' This function generates simulated coefficients based on coefficient means and covariances.
#' @param obsval_obj The results from obsval() or obsvalPredict(). 
#' @param dv_category If the dependent variable is categorical, this specifies which category's probabilities to graph.
#' @export
#' @examples
#' library(mvtnorm)
#' library(MASS)
#' library(ggplot2)
#' 
#' 
#' data       	<- quine
#' specification 		<- 'Days~Eth+Sex+Age'
#' reg.model 			<- "negbin"
#' n.draws				<- 100
#' effect.var			<- "Age"
#' effect.var.high		<- "F3"
#' effect.var.low		<- "F0"
#' 
#' mymodel <- obsval( data, specification, reg.model, n.draws, 
#'                    effect.var, effect.var.low,effect.var.high,
#'                    options=list(verbose=TRUE,
#'                                  effect_range="full"))
#' 
#' graph_range( mymodel ) 


graph_range <- function( obsval_obj, dv_category=NULL ){
  
  # x-values
  if( obsval_obj$effect_is_factor == TRUE ){
    xrange <- factor(obsval_obj$effect_range, labels=obsval_obj$orig_effect.vals)
  } else {
    xrange <- obsval_obj$effect_range
  }
    
  # y-values
  if( obsval_obj$reg.model=="ologit" || 
        obsval_obj$reg.model=="oprobit" || 
        obsval_obj$reg.model=="mlogit" ){
      yrange <- c(obsval_obj$means[,dv_category],
                  obsval_obj$low.ci[,dv_category],
                  obsval_obj$high.ci[,dv_category] )     
  } else {
    yrange <- c(obsval_obj$means[1,],
                obsval_obj$low.ci[1,],
                obsval_obj$high.ci[1,] ) 
  }
    
  # y-axis label
  if( obsval_obj$reg.model=="logit" || 
        obsval_obj$reg.model=="probit" ||
        obsval_obj$reg.model=="clogit" ){
    ylabel <- paste("Pr(",obsval_obj$dep_var,sep="")
    ylabel <- paste(ylabel,")",sep="")
  } else if(obsval_obj$reg.model=="ologit" || 
              obsval_obj$reg.model=="oprobit" ||
              obsval_obj$reg.model=="mlogit" ){
    ylabel <- paste("Pr(",obsval_obj$dep_var,sep="")
    ylabel <- paste(ylabel,"=\'",sep="")
    ylabel <- paste(ylabel,dv_category,sep="")
    ylabel <- paste(ylabel,"\')",sep="")
  }
  else{
    ylabel <- obsval_obj$dep_var
  }
  
  # set data
  n.effect.vals <- length(obsval_obj$effect_range)
  legend_data <- c(rep("mean",n.effect.vals), 
                   rep("low", n.effect.vals), 
                   rep("high",n.effect.vals))
  
  # merge data
  mydata <- data.frame(xrange = xrange, 
                       yrange = yrange,
                       legend = legend_data) 
  
  # plot 
  p<-ggplot(mydata, aes( xrange, yrange, group=legend, colour=legend)) 
  
  p <- p + geom_line() + 
    xlab( obsval_obj$effect.var ) + 
    ylab( ylabel ) 
  
  p 
  
}

