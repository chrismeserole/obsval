context("Clustered SEs test with logit")
set.seed(123)
n <- 1000
b0 <- 0.2
b1 <- -1
b2 <- 2
x1 <- rnorm(n)
x2 <- rnorm(n)
cluster.id <- rep(seq(1:100),10)
cluster.vals <- rep(rnorm(100,0,5),10)
eta <- rnorm(n,0,.1)
Xb <- b0+b1*x1+b2*x2+cluster.vals
y.prob <- exp(Xb)/(1+exp(Xb))
summary(y.prob)
y <- rbinom(n,1,y.prob)

df <- data.frame(y,x1,x2,cluster.id)
mymodel1 <- obsval(y~x1+x2, df, reg.model = "logit", cluster.se="cluster.id",
                   n.draws=100,
                   effect.var="x2", effect.vals=c(0,1), verbose=TRUE)  

(mymean1 <- round( mean(mymodel1$effect.preds), 6))

test_that("logit with clustered SEs mean predicted probability", {
  expect_equal( mymean1, 0.167401 )
})


x2[x2<.6 & x2>.3] <- NA
df <- data.frame(y,x1,x2,cluster.id)
 
mymodel2 <- obsval(y~x1+x2, df, reg.model = "logit", cluster.se="cluster.id", 
                   n.draws=100,
                   effect.var="x2", effect.vals=c(0,1,2), verbose=TRUE)

(mymean2 <- as.numeric(round( quantile(mymodel2$effect.preds,0.24), 6)))

test_that("logit with clustered SEs mean predicted probability and missing data", {
  expect_equal( mymean2, 0.282864 )
})