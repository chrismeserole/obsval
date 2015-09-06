context("Multiple effect var vals with OLogit")
mymodel <- obsval(Exer~Sex+Smoke+Age, 
                  data = MASS::survey,
                  reg.model = "ologit",
                  n.draws = 100,
                  effect.var = "Smoke", 
                  effect.vals = c("Heavy","Never","Occas"),
                  verbose = TRUE)

(tstr <- round(mean(mymodel$preds[,2,3]-mymodel$preds[,2,1]),8))

test_that("ologit predicted probability for first category of dv", {
  expect_equal( tstr, -0.0008592 )
})