[![Travis-CI Build Status](https://travis-ci.org/chrismeserole/obsval.png?branch=master)](https://travis-ci.org/chrismeserole/obsval)

### Description

This package calculates predicted probabilities by using observed covariate values rather than mean covariate values. It implements Hanmer and Kalkan, "Behind the Curve: Clarifying the Best Approach to Calculating Predicted Probabilities and Marginal Effects from Limited Dependent Variable Models", American Journal of Political Science (2013).

### Example

To use `obsval`, you first need to install it via Github, which you can do as follows: 

    install.packages("devtools")
    library("devtools")
    devtools::install_github("chrismeserole/obsval")
    library("obsval")
    
To run a multinomial model, as we will in this example, we also need to install `mvtnorm` and `nnet`:

    install.packages("mvtnorm")
    install.packages("nnet")
    library(mvtnorm)
    library(nnet)
  
Now that everything's installed and loaded, we can assign the following parameters:

    data     		      <- studentVote
    specification 		<- 'warsup~female'
    reg.model 			  <- "mlogit"
    n.draws				    <- 1000
    effect.var			  <- 'female'
    effect.var.high	  <- 1
    effect.var.low		<- 0
    verbose 			    <- TRUE
    baseline.category	<- "strongly oppose"

The dataset specified, `studentVote`, is lazy-loaded whenever obsval() is called. 

The `warsup` variable is categorical, and describes whether a given respondent to a survey supported the Iraq war. Its categories include `strongly oppose`, `somewhat oppose`, `somewhat support` and `strongly support`.

The `female` variable indicates whether a respondent was either male (`0`) or female (`1`). 

Crucially, by setting `effect.var` to `female`, `effect.var.high` to `1`, and `effect.var.low` to `0`, we are telling obsval to calculate the effect of switching from male to female on the likelihood that a respondent supports the Iraq war at a particular level.

Now that all parameters have been specified, we can finally run `obsval` itself:

    mymodel <- obsval( data, specification, reg.model, n.draws,
                      effect.var, effect.var.high, effect.var.low, 
                      verbose, baseline.category )

To get the average effect of our variable of interest, we can then run the following:

    mean(mymodel$pp_effect[,'strongly oppose'])
    
However, we don't just want the average effect. We also want to know how certain we are about the predicted effect. 

To approximate a 95% confidence interval for our estimated effect, we should also run: 
    
    quantile(mymodel$pp_effect[,'strongly oppose'], c(0.025, 0.975))
    
If the quantile values are both above or both below 0, we can be confident in saying that our variable of interest actually is producing a significant effect. In this case, both quantiles should be above 0, meaning that women should be more likely to strongly oppose the Iraq war than men.

In addition to numeric estimates, we can also quickly visualize the results. To view the predicted effect for each draw from the posterior distribution, run the following: 
  
    plot(mymodel$pp_effect[,'strongly oppose'])

To view a summary of the data, instead run:

    boxplot(mymodel$pp_effect[,'strongly oppose'])

All these commands can be found in a [gist here](https://gist.github.com/chrismeserole/3817d693153c5c688fb9). 
