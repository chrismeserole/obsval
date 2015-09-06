mymodel23 <- obsval(Exer~Sex+Smoke+Age, 
                    data = MASS::survey, 
                    reg.model = "oprobit",
                    effect.var = "Sex", 
                    effect.vals = c("Female","Male"), 
                    n.draws = 100,
                    verbose=TRUE)


(mymean <- round( mean(mymodel23$effect.preds[,'Freq']), 7 ))
test_that("oprobit predicted probability for first category of dv", {
  expect_equal( mymean, 0.1459744 )
})