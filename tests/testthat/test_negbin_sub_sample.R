context("Negbin Sumsample Test")

moda <- obsval(Days~Eth+Sex+Age, data=MASS::quine, reg.model="negbin", 
               effect.var="Age",
               effect.vals=c("F1","F3"),
               subsample.var="Sex",
               subsample.val = "M",
               verbose=TRUE)

(mymean <- round(moda$effect_sum[,2], 6))

test_that("negbin mean predicted effect for subsample", {
  expect_equal( mymean, 4.618012 )
})