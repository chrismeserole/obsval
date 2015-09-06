#' Factor Object To Numeric Object Conversion
#'
#' This function converts categorical vectors to numeric vectors with the 
#' lowest category set to 0. If a factor is passed to obsval() as the 
#' effect.var, this function is used to convert it to numeric values.  It 
#' returns both the converted vector and a key-value list that maps factor 
#' levels to their corresponding numeric values. See example below for more.
#' @param vec A vector of elements, typically with class factor
#' @examples
#' 
#' # create factor vector
#' gender <- factor(c("male","female","male","male"))
#' 
#' # run factor2numeric
#' convert <- factor2numeric( gender )
#' 
#' # retrieve numeric vector
#' (gender <- convert$numvec)
#' 
#' # retrieve numeric values corresponding to initial factor levels 
#' (male.val <- convert$keys[["male"]])
#' (female.val <- convert$keys[["female"]])

factor2numeric <- function( vec ){
  if( is.character( vec ) ){
    cat(' factor2numeric() requires a factor object. The input received is a character object.' )
    stop()
  }
  else if( is.numeric( vec ) ){
    return( list(numvec=vec ))
  }
  else if( is.factor( vec ) ){

    # convert input vector to numeric vector
    newvec <- as.numeric( vec) - 1

    # get all unique values of input vector & new vector
    unique_vec <- unique( vec )
    unique_newvec <- unique(newvec)

    # initialize relevant lists
    index_position.list <- list()
    output_list <- list()
    
    # loop through each unique value of input vector, and store 
    # index positions of all times that value appears in input vector 
    for( i in 1:length(unique_vec) ){      
      index_position.list[[ as.character(unique_vec[i]) ]] <- which(vec == unique_vec[i]) 
    }

    # loop through each unique value of new vector
    for( i in 1:length( unique_newvec ) ){
      
      # store index positions of all times current value apperas in new vector
      index_positions_vec <- which( newvec == unique_newvec[i] )

      # loop through unique values of input vector
      for( j in 1:length( unique_vec ) ){

        # if index positions match, store in output_list
        if( identical(index_position.list[[ unique_vec[j] ]], index_positions_vec) ) {
          output_list[[ as.character(unique_vec[j]) ]] <- unique_newvec[i]
        }
      }
    }
    return(list(numvec=newvec,keys=output_list))
  }
  else{ 
    message(paste('factor2numeric() requires factor object. The object passed is of type: ', typeof(vec), sep="" ))
    stop()
  }
}

confirmEffectVar <- function(effect.var, fmla){
  # make sure effect variable is actually in fmla
  if(!(effect.var %in% all.vars(fmla)[-1])){
    message('Effect variable is not in specified model.')  
    stop(options(show.error.messages=FALSE))
  } 
}

# cleans effect var & returns appropriately typed effect var
cleanFactorVar <- function( vec, var.vals, vartype, verbose ){

  is_factor <- FALSE
  
  # if var class is character, convert to factor
  if( is.character(vec)) vec <- factor(vec)
  if( is.factor(vec) ) {
    is_factor <- TRUE       
    confirmFactorVals( vec, var.vals, vartype)
  }
  # ensure numeric
  vec <- factor2numeric( vec )

  vec$is_factor <- is_factor
  
  # if effect var is factor, replace with numeric values
  if( vec$is_factor==TRUE ){
    if( verbose == TRUE ) {
      cat('Effect variable is factor. Resetting to numeric.')
    }
    
    for(i in 1:length(var.vals)){
      var.vals[i] <- vec$keys[[ as.character(var.vals[i])]]
    }    
  }  
    
  # return list object
  return(list(vec=vec$numvec, vals=var.vals))
}

# cleans effect var & returns appropriately typed effect var
confirmFactorVals <- function( vec, vals, vartype ){
  for(i in 1:length(vals)){
    confirmVal(vec, vals[i], vartype)
  }
  return()
}

# confirm value exists in vector
confirmVal <- function( vec, val, name_var ){
  if( !(val %in% vec)){
    str <- paste(name_var, " does not contain: ", sep="")
    message(paste(str, val, sep="") )
    stop(options(show.error.messages=FALSE))
  }
  return()
}

# confirms range of function
confirmEffectVarRange <- function( vec, high.val, low.val ){
  
  # interactive responses
  responseFunc <- function(x){
    if(x == 'c'){
      cat("Continuing with high and low values specified ... \n")
      return()
    }
    else if(x == 't'){
      s <- "Exiting the function. Please re-specify your high and low "
      s <- paste(s, "variables and try again.")
      message(s)
      stop(options(show.error.messages=FALSE))
    }    
    else if(x != 'm' || x != 'c' || x!= 't'){
      cat("Could not understand your response. Please try again.")
      stop(options(show.error.messages=FALSE))
    }
  }
  
  n.unique.vals   <- length(unique(vec))
  effect.var_binary <- FALSE
  
  if( !is.factor(vec) ){
    effect.var_categorical <- FALSE
  } else {
    effect.var_categorical <- TRUE
  }
  
  # compute min and max 
  effect.var_min  <- min(vec)
  effect.var_max  <- max(vec)
  
  # flag if effect variable binary
  if(n.unique.vals==2){
    if( effect.var_min==0 & effect.var_max==1 ){
      if(verbose==TRUE){
        cat("Effect variable is binary.\n")
      }
      effect.var_binary <- TRUE
    }
    if( effect.var_min==1 & effect.var_max==2 ){
      if(verbose==TRUE){
        cat("Effect variable is binary.\n")
      }
      effect.var_binary <- TRUE
    }
  }
  
  
  # check feasibility of range
  #   if(!effect.var_binary)
  # 	{
  # 	  effect.var_min  <- min(data[[effect.var]])
  # 	  effect.var_max  <- max(data[[effect.var]])
  # 	  effect.var.mean <- mean(data[[effect.var]])
  # 	  effect.var_sd   <- sd(data[[effect.var]])
  # 	  
  # 	  effect.var_min.max_range	<- abs(effect.var_min-effect.var_max)
  # 	  effect.var_estimate_range	<- abs(effect.var.high-effect.var.low)
  # 	  
  # 		if(effect.var_estimate_range > effect.var_min.max_range)
  # 		{
  # 			cat("The range between the high and low values you specified for the effect variable is greater than the range between its max and min values. Do you want to continue (enter 'c'), or specify new values and try again (enter 't')?")
  # 			responseFunc(readLines(n=1))
  # 		}
  # 		else if(effect.var_estimate_range == effect.var_min.max_range)
  # 		{
  # 			cat("The effect range you've specified moves from the mininum value of your effect variable to the maximum value. In many contexts, such a movement isn't feasible. Do you still want to continue (enter 'c'), or specify new values and try again (enter 't')?")
  # 			responseFunc(readLines(n=1))
  # 		}
  #     
  # 		else if(effect.var_estimate_range > effect.var_sd & !effect.var_categorical)
  # 		{
  # 			cat("The range between the high and low values you specified for the effect variable is greater than one standard deviation of your effect variable. In some contexts, such a movement isn't feasible. Do you still want to continue (enter 'c'), or specify new values and try again (enter 't')?")
  # 			responseFunc(readLines(n=1))
  # 		}		
  # 	}
}


# concatenates two objects (forces numeric to character)
concat <- function( arg1, arg2 ){
  return(paste(as.character(arg1),as.character(arg2),sep=""))
}

# generates summary table of effect
effectSum <- function( effect.low.ci, effect.mean, effect.high.ci,
                       low.ci.val, high.ci.val ){
  
  # for use as colname in effect_sum
  ciTxt <- function( val ){
    ci_txt <- 100*val
    return( concat(ci_txt, "%") )
  }
  
  # for logit, probit, etc
  if( is.null(dim(effect.mean))){
    effect_sum <- cbind( effect.low.ci, effect.mean, effect.high.ci )
    colnames(effect_sum) <- c(ciTxt(low.ci.val), "Mean", ciTxt(high.ci.val))
    rownames(effect_sum) <- c("")
    # for ologit, oprobit, mlogit
  } else {
    effect_sum <- matrix(NA, nrow=length(effect.mean[1,]), ncol=3)
    for(i in 1:length(effect.mean[1,])){
      
      effect_sum[i,] <- c(effect.low.ci[,i],
                          effect.mean[,i],
                          effect.high.ci[,i])
      
    }
    colnames(effect_sum) <- c(ciTxt(low.ci.val), "Mean", ciTxt(high.ci.val))
    rownames(effect_sum) <- colnames(effect.mean)
  }    
  return(effect_sum)
}

getSubset <- function( data, var, val ){
  return(subset(data, data[,var]==val))
}

getMeansAsMatrix <- function( data, n.cats ){
  return(matrix(apply(data,2, function(x) mean(x)),ncol=n.cats))
}

getCIsAsMatrix <- function( data, ci.val, n.cats ){
  return(matrix(apply(data,2, function(x) quantile(x,ci.val)),ncol=n.cats))
} 

getNumDVCategories <- function( model, reg.model ){
  n.dv.cat <- NULL
  if( reg.model=='ologit' || reg.model=='oprobit'){
    n.dv.cat <- length(model$zeta)+1  
  } else if( reg.model=='mlogit'){
    n.dv.cat <- length(model$lab)  
  }
  return(n.dv.cat)
}


#
# PRIVATE FUNCTIONS ---------------------------------------------------------
#

# ensures the DV is of appropriate type for reg.model.
# returns either input DV or appropriately typed DV.
cleanDV <- function( y, reg.model, verbose ){

  # make sure that binary DVs are in fact binary
  if(reg.model=="probit" || reg.model=="logit" || reg.model=="clogit"){
    binary <- confirmBinary( y )
    if( binary == TRUE){
      if(verbose==TRUE){
        cat('Dependent variable is numeric and binary.\n')
      }
    } else {
      s <- concat('The reg.model specified requires a binary dependent', 
                  concat('variable.\nPlease check your specified dependent ',
                         'variable or model type and try again.\n\n'))
      message(s)
      stop(options(show.error.messages=FALSE))
    }
  }
  
  # make sure that categorical DVs are factors 
  else if(reg.model=="ologit" || reg.model=="oprobit" || reg.model=="mlogit"){
    
    if(!is.factor(y)){
      # if all dv values are integers ... 
      if( confirmIntegers(y) == TRUE ){
        
        # coerce data to factor
        vec <- factor(vec)
        
        if(verbose==TRUE){
          cat('Coerced dependent variable to factor class')
        }
        
        # if not all dv values are integers, throw error
      } 
      else {
        s <- 'Not all values of the specified dependent variable are integers, '
        s <- paste(s, 'and thus should not be classed as categorical values. ')
        s <- paste(s, 'Please check your dependent variable and try again.\n')
        message(s)
        stop(options(show.error.messages=FALSE))
      }  
    } 
    else {
      if(verbose==TRUE){
        cat('Dependent variable is categorical.\n')
      }
    }
  }
  return(y)
}

# concatenates two objects (forces numeric to character)
concat <- function( arg1, arg2 ){
  return(paste(as.character(arg1),as.character(arg2),sep=""))
}

# confirms baseline.category is set 
confirmBaselineCategory <- function( baseline.category ){
  if( is.null(baseline.category) ){
    message("Please specify baseline.category and try again.")
    stop(options(show.error.messages=FALSE))
  }
}

# confirms that a vector is binary. returns TRUE/FALSE.
confirmBinary <- function( vec ){
  
  # get vector of unique values in vec, as well as its len, max and min
  unique_vec <- uniqueValues( vec ) 
  
  if(unique_vec$len==2 & unique_vec$min==0 & unique_vec$max==1){
    return(TRUE)
  } 
  else if (is.factor(vec) & nlevels(vec) == 2){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

# confirms that a vector is integer. returns TRUE/FALSE.
confirmIntegers <- function( vec ){
  if( is.integer( vec ) ){
    return(TRUE)
  }
  else if( all((vec-round(vec))==0) ){
    
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

# returns list with vector of unique values, as well as len, min & max
uniqueValues <- function( vec ){
  
  # this returns vector of unique values in ascending order of freq.
  # for large vectors, it's more computationally efficient than 
  # calling unique(data),max(data),min(data)
  unique_vec <- unique(sort(vec))
  n.unique_vec <- length(unique_vec)
  
  # return list of relevant values
  return(list(vec=unique_vec, 
              len=n.unique_vec,
              min=unique_vec[1],
              max=unique_vec[n.unique_vec]))
}


