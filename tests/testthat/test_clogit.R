context("Conditional Logit Test")

# run the model + get estimates
mymodely <- obsval("case~spontaneous+induced+strata(stratum)",
                  data = infert,
                  reg.model = "clogit", 
                  n.draws = 100,
                  effect.var = "spontaneous", 
                  effect.vals = c(0,1),
                  verbose = TRUE)

(mymean <- round( mean(mymodely$effect.preds), 7 ))

test_that("conditional logit mean predicted probability", {
  expect_equal( mymean, 0.2700595 )
})

