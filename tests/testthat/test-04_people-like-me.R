test_that("multiplication works", {

  library(plmlmm)
  data("epic_train")
  data("epic_test")
  outcome <- people_like_us(train_data = epic_train,
                 test_data = epic_test,
                 anchor_time = c(6, 12),
                 brokenstick_knots = c(5, 10, 15),
                 match_methods = "euclidean",
                 match_number = 5,
                 weight = FALSE,
                 match_plot = TRUE,
                 predict_plot = TRUE)
  View(outcome)
}

)
