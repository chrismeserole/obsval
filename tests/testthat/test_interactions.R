set.seed(123)

n <- 100

b0 <- 3
b1 <- 5
b2 <- -3
b3 <- 10
x1 <- rnorm(n)
x2 <- rnorm(n)
eta <- rnorm(n,0,10)

y <- b0 + b1*x1 + b2*x2 + b3*x1*x2 +  eta 

df <- data.frame(y,x1,x2)

library(foreign)
write.dta(df,"ols-interaction.dta")
mod1a <- obsval(y~x1+x2+x1*x2, df, reg.model="ols")
mod1b <- obsval(y~x1+x2+x1*x2, df, reg.model="ols", effect.var="x1", effect.vals=c(0,10),verbose=T)

(tst1 <- round(as.numeric(mod1a$coef[2]),6))
(tst2 <- round(min(mod1b$preds[,1])[1]))

test_that("ols with interaction but no effect var specified", {
  expect_equal( tst1, 4.071923 )
})

test_that("ols wiht interaction and effect var specified", {
  expect_equal( tst2, 2  )
})

#
#
#

set.seed(123)
n <- 100
b0 <- -.4; b1 <- .2; b2 <- -.8; b3 <- 1
x1 <- rnorm(n); x2 <- rnorm(n); eta <- rnorm(n,0,1)
xb <- b0 + b1*x1 + b2*x2 + b3*x1*x2 +  eta 
y <- rbinom(n,1,exp(xb)/(1+exp(xb)))

df <- data.frame(y,x1,x2)
mod1 <- obsval(y~x1+x2+x1:x2,df,reg.model="probit")

mod2 <- obsval(y~x1+x2+x1:x2,df,reg.model="probit",
               effect.var="x2",
               effect.vals=c(0,1),
               verbose=T)

(tstr3 <- round(as.numeric(mod1$coef[1]),3))
(tstr4 <- round(max(mod2$preds[,1]),3))

test_that("probit with interaction but no effect var specified", {
  expect_equal( tstr3, 0.149 )
})

test_that("probit wiht interaction and effect var specified", {
  expect_equal( tstr4, 0.7  )
})

