#context("MLogit Test")
# 
# mymodel <- obsval( Exer~Sex+Smoke+Age, 
#                    data = MASS::survey, 
#                    reg.model = "mlogit", 
#                    n.draws = 100,
#                    effect.var = "Sex", 
#                    effect.vals = c("Female", "Male"), 
#                    baseline.category = "None",
#                    verbose = TRUE)
# 
# (mymean <- round( mean(mymodel$effect.preds[,'Freq']), 7 ))
# 
# test_that("mlogit mean predicted probability", {
#   expect_equal( mymean, 0.1075144 )
# })
