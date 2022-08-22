

get_plm_lmm <- function(dataset) {
  train <- filter(group == train)
  test <- filter(group == test)

  lmm0 <- lme(ht ~ bs(time, knots = c(10, 12, 15), degree = 3) * sex - 1,
              random = ~ 1 + bs(time, df = 5, degree = 2, intercept = FALSE)| id,
              control = ctrl,
              data = all)


  lmm_results <- lmm_pred(fitting = lmm0,
                          test_baseline = test_baseline)

}


































