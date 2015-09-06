
computePreds <- function( model, reg.model, sim.coefs, 
                          effect.var, effect.var.value,
                          X.matrix, col.names, verbose=FALSE ){
    
  if(verbose==T) cat("Entered computePreds()... \n")

  if(length(dim(sim.coefs)) == 2 ) {
    n.draws <- length(sim.coefs[,1])
  }
  else {
    n.draws <- length(sim.coefs[,1,1])
  }
  
  n.obs <- length(X.matrix[,1])

  if( effect.var.value != "no_manipulation" ){
    
    # Create replacement vectors based on high and low values
    effect_vector <- rep(as.numeric(effect.var.value),n.obs)

    # Create predictor matrices using high and low values
    X.matrix[,effect.var] <- effect_vector
  }
  # need to set effect_vector for control preds as well, in case the effect var
  # is part of an interaction term
  else {
    effect_vector <- X.matrix[,effect.var]
  }
  
  #
  # INTERACTIONS --------------------------------------------------------------
  #
  
  # For each X.matrix, recalculate interaction terms that have effect variable
  interaction.symbol <- ':'
  interaction.columns <- grepl(interaction.symbol,colnames(X.matrix))
  
  # loop through grepl result for each term in model
  if(TRUE %in% interaction.columns){
    for(k in 1:length(interaction.columns)){
      
      # if the column contains an interaction, 
      # identify its constituent variables
      if(interaction.columns[k]==TRUE){
        ia_vars <- strsplit(as.character(colnames(X.matrix)[k]), 
                            interaction.symbol) #returns list
        
        ia_vars <- matrix(unlist(ia_vars),
                          ncol=length(ia_vars[[1]]))
        
        # if one of the interaction variables is our effect var, 
        # then recreate the interaction column with the specified 
        # value(s) for our effect var 
        if(effect.var %in% ia_vars){
          X.matrix[,k]   <- effect_vector
          for(z in 1:ncol(ia_vars)){  	
            if(ia_vars[1,z] != effect.var){
              X.matrix[,k] 	<- X.matrix[,k] * X.matrix[,ia_vars[1,z]]
            }
          }
        }
      }
    }
  }
  
  
  #
  # REFORMAT X_MATRICES FOR MODELS WITH NO CONSTANT ---------------------------
  # note: must do after handling interactions
  #
  
  if(reg.model=='oprobit' || reg.model=='ologit'){
    if( dim(X.matrix)[2] == 2 ){
      nocons_X_vector <- X.matrix[,2]
    }
    else{
      nocons_X.matrix <- X.matrix[,2:dim(X.matrix)[2]]
    }
  }
  

  #
  # INITIALIZE VECTOR/MATRIX USED TO STORE MEAN PREDICTIONS -------------------
  #
  
  # Create an empty vector to store mean predicted 
  # probabilities for each set of coefficients. 
  p.mean <- numeric(n.draws)
  
  # If ordered/unordered data, create empty matrix to store mean
  # predicted probabilities for each set of coefficients
  if(reg.model=="ologit" || reg.model=="oprobit"){
    n.cuts 			<- length(model$zeta) #assumes model estimated using polr()
    n.coefs 		<- length(model$coefficients)
    p.mean <- matrix(nrow=n.draws,ncol=n.cuts+1)
  } else if(reg.model=="mlogit"){
    n.coefs <- dim(coef(model))[2]
    n.coef.sets <- dim(coef(model))[1]
    p.mean <- matrix(nrow=n.draws,ncol=n.coef.sets+1)
  }
  
  
  if(reg.model=="ologit" || reg.model=="oprobit" || reg.model=="mlogit"){
    colnames(p.mean) <- col.names
  }

  
  #
  # COMPUTE XB AND PREDICTIONS ------------------------------------------------
  #
  for(i in 1:n.draws){ 
    
      
    # 
    # GENERATE Xb -------------------------------------------------------------
    #
    
    if(  reg.model=="ols"    ||
         reg.model=="probit" || 
         reg.model=="logit"  || 
         reg.model=="clogit" ||
         reg.model=="negbin" || 
         reg.model=="poisson" ){
      
      # For the current set of coefficients, calculate a
      # latent probability for all observations using observed values
      
      # First, calculate the linear predictor
      if( dim(X.matrix)[2] == 1 ) {
        Xb	<- X.matrix * sim.coefs[i,]
      } else {
        Xb 	<- X.matrix %*% sim.coefs[i,]
      }
    }
    
    else if(reg.model=="ologit" || 
              reg.model=="oprobit"){
      
      # First, calculate the partial linear predictor (ie, the Xb)
      if( dim(X.matrix)[2] == 2 ) {
        Xb_partial 	<- nocons_X_vector * sim.coefs[i,1:n.coefs]
      }
      else {
        Xb_partial 	<- nocons_X.matrix %*% sim.coefs[i,1:n.coefs]
      }
      
    }	
    
    else if(reg.model=="mlogit"){
      
      n.coefs <- dim(coef(model))[2]
      n.coef.sets <- dim(coef(model))[1]
      
      # create vector to store linear predictor for each category of DV
      Xb <- matrix(nrow=n.obs,ncol=n.coef.sets)
      
      # get linear predictors 
      for(j in 1:n.coef.sets){
        Xb[,j] <- X.matrix %*% sim.coefs[i,,j]
      }
    }		
    
    
    # 
    # TRANSFORM Xb --------------------------------------------------------------
    #
    if( reg.model=="ols") {
      p.mean[i]   <- mean(Xb,na.rm=TRUE) 
    }
    
    if(reg.model=="probit" || 
         reg.model=="logit" || 
         reg.model=="clogit" ||
         reg.model=="negbin" || 
         reg.model=="poisson"){
      
      if(reg.model=="probit"){
        pred_vec	<- pnorm(Xb)
      } else if(reg.model=="logit" || 
                  reg.model=="clogit"){
        pred_vec	<- exp(Xb)/(1+exp(Xb))
      } else if(reg.model=="negbin"){
        pred_vec	<- MASS::rnegbin(exp(Xb),theta=model$theta)
      } else if(reg.model=="poisson"){
        pred_vec	<- round(exp(Xb),0)
      }
      
      p.mean[i] 	<- mean(pred_vec,na.rm=TRUE) 
      
    } 
    
    # ordered models
    else if(reg.model=="ologit" || reg.model=="oprobit"){
      
      pred_mat	 	<- matrix(nrow=n.obs,ncol=n.cuts+1)
      cuts_mat    <- matrix(nrow=n.obs,ncol=n.cuts)
      
      for(k in 1:n.cuts){
        cut_k_vector <- rep(sim.coefs[i,n.coefs+k],n.obs)
        if(reg.model=="ologit"){
          cuts_mat[,k] <- (1 / (1 + exp(-( cut_k_vector - Xb_partial ))))
        } else if(reg.model=="oprobit"){
          cuts_mat[,k] 		<- pnorm( cut_k_vector - Xb_partial )
        }					
      }
      
      pred_mat[,1] <- cuts_mat[,1]
      for(k in 2:n.cuts){
        pred_mat[,k] 	<- cuts_mat[,k] 	- cuts_mat[,k-1]
      }
      pred_mat[,n.cuts+1] 	<- 1 - cuts_mat[,n.cuts]
      
      if(verbose == TRUE & i%%100 == 0){
        cat('Iterations completed: ')
        cat(i)
        cat('\n')
      }
      
      for(k in 1:(n.cuts+1)){
        p.mean[i,k] <- mean(pred_mat[,k])
      }			
    }	
    
    # multinomial models
    else if(reg.model=="mlogit"){
      
      predict_mat 		<- matrix(nrow=n.obs,ncol=n.coef.sets+1)
      
      n.coefs <- dim(coef(model))[2]
      n.coef.sets <- dim(coef(model))[1]
      
      # denominator for all choices is 1 + exp(linear_pred_1) + 
      #                             exp(linear_pred_2) + exp(linear_pred_n)
      # note: for apply(), the '1' tells apply() to execute the function over each
      # row of matrix specified. thus, this produces vectors of length n.obs
      mlogit_denominator <- 1 + apply(Xb, 1, function(x) sum(exp(x)) )
      
      # get predictions for non-baseline categories
      # note that numerator for given choice j is exp(X*B_j)
      for(j in 1:n.coef.sets){
        predict_mat[,j]   <- exp(Xb[,j])/mlogit_denominator
      }
      
      # get predictions for baseline category
      predict_mat[,n.coef.sets+1] <- 1/mlogit_denominator
      
      
      if(verbose == TRUE & i%%100 == 0){
        cat('Iterations completed: ')
        cat(i)
        cat('\n')
      }
      
      # store mean across all observations for current set of coefs
      for(j in 1:(n.coef.sets+1)){
        p.mean[i,j] 	<- mean(predict_mat[,j])
      }			
    }		
  }      
  return(p.mean)
}
