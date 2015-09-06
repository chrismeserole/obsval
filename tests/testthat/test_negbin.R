context("Negbin Test")

# run the model + get estimates
mymodel <- obsval( Days~Eth+Sex+Age, 
                   data = MASS::quine, 
                   reg.model = "negbin",
                   effect.var = "Age", 
                   effect.vals = c("F0","F3"),
                   n.draws = 100,
                   verbose = TRUE )

(mymean <- round( mean(mymodel$effect.preds), 6 ))

#library(ggplot2)
#graph_range(mymodel)

# test_that("negbin mean predicted counts", {
#   expect_equal( mymean, 6.492671 )
# })
