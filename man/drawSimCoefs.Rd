% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/drawsimcoefs.R
\name{drawSimCoefs}
\alias{drawSimCoefs}
\title{Generate Simulated Coefficients}
\usage{
drawSimCoefs(model, reg.model, n.draws = 1000, verbose = FALSE,
  seed = 123)
}
\arguments{
\item{model}{A model object. For ordered regression, make sure the cutpoints are appended to model$coef.}

\item{reg.model}{A string containing the type of regression to run. Available options are logit, clogit, ologit, mlogit, probit, oprobit, poisson, negbin.}

\item{n.draws}{An integer specifying how many draws of coefficient estimates to make.}

\item{options}{A list of optional variables, including verbose and seed.}
}
\description{
This function generates simulated coefficients based on coefficient means and covariances.
}
\examples{
library(arm)
library(mvtnorm)

n.draws <- 100
reg.model <- "logit"

model <- glm('binary_warsup~female+partyid', family=binomial(link="logit"),
               data=studentVote)

sim.coefs <- drawSimCoefs(model, reg.model, n.draws)

head(sim.coefs)
}

