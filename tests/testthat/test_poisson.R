context("Poisson Test")

# run the model + get estimates
mymodelz <- obsval( Days~Eth+Sex+Age, 
                   data = MASS::quine,
                  reg.model = "poisson",
                  n.draws = 100, 
                   effect.var = "Eth", 
                  effect.vals = c("A","N") )

(mymean <- round( mean(mymodelz$effect.preds), 5 ))

test_that("poisson mean predicted counts", {
  expect_equal( mymean, -8.78438 )
})
