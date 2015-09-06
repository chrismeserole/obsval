mymodel <- obsval(Exer~Sex+Smoke+Age, 
                  data = MASS::survey,
                  reg.model = "ologit",
                  n.draws = 100,
                  effect.var = "Sex", 
                  effect.vals = c("Female","Male"),
                  verbose = TRUE)

(mymean <- round( mean(mymodel$effect.preds[,'Freq']), 7))

test_that("ologit predicted probability for first category of dv", {
  expect_equal( mymean, 0.1443999 )
})

df <- MASS::survey
table(df$Smoke)
