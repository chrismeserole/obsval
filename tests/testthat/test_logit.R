context("Logit Test")

# run the model + get estimates
mymodel <- obsval(binary_warsup~female+partyid,
                  data = studentVote, 
                  reg.model = "logit",
                  n.draws = 100,
                  effect.var = "female", 
                  effect.vals = c(0,1),
                  verbose = TRUE)

(mymean <- round( mean(mymodel$effect.preds), 8 ))

test_that("logit mean predicted probability", {
  expect_equal( mymean, 0.02308802 )
})
