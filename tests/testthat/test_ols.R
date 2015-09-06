set.seed(123)

n <- 100

b0 <- 3
b1 <- 5
b2 <- -3
x1 <- rnorm(n)
x2 <- rnorm(n)
eta <- rnorm(n,0,10)

y <- b0 + b1*x1 + b2*x2 +  eta 

df <- data.frame(y,x1,x2)
mod1a <- obsval(y~x1+x2, df, reg.model="ols")
mod1b <- obsval(y~x1+x2, df, reg.model="ols", effect.var="x1", effect.vals=c(0,10))

(tst1 <- round(as.numeric(mod1a$coef[1]),6))
(tst2 <- round(mean(mod1b$control.preds),6))
(tst3 <- round(mean(mod1b$effect.preds),1))

test_that("ols with no effect var specified", {
  expect_equal( tst1, 4.350654 )
})

test_that("control preds for ols with effect var specified", {
  expect_equal( tst2, 4.967897 )
})

test_that("effect preds for ols with effect var specified", {
  expect_equal( tst3, 37.1 )
})


# create clustered data
cluster.id <- rep(1:10,10)
cluster.vals <- rep(rnorm(10,0,10),10)

y2 <- b0 + b1*x1 + b2*x2 + cluster.vals 
df2 <- data.frame(y2,x1,x2,cluster.id)

mod2 <- obsval(y2~x1+x2, 
               data=df2, 
               reg.model="ols",
               effect.var="x1",
               effect.vals=c(0,10),
               cluster.se="cluster.id")

(tst4 <- round(as.numeric(mod1a$coef[1]),6))
(tst5 <- round(mean(mod1b$control.preds), 6))
(tst6 <- round(mean(mod1b$effect.preds),1))

test_that("ols with no effect var specified", {
  expect_equal( tst4, 4.350654 )
})

test_that("control preds for ols with effect var specified", {
  expect_equal( tst5, 4.967897 )
})

test_that("effect preds for ols with effect var specified", {
  expect_equal( tst3, 37.1 )
})

