library(testthat)
library(plmlmm)
library(tictoc)

# context("Tests related to impute functions")
test_that("impute of brokenstick anchor1", {

  data("epic_train")
  data("epic_test")
  anchor_time <- c(5, 10, 12, 14)

  tic()
  data_bsk_impute <- impute_brokenstick(outcome_var = "ht",
                                        time_var = "time",
                                        id_var = "id",
                                        bs_knots = c(5, 10, 15),
                                        anchor_time = anchor_time,
                                        data = epic_train)
  toc()
  ## 10.244 sec elapsed for 4 anchor time
  ## 10.361 sec elapsed for 6 anchor time
  ## 10.369 sec elapsed for 8 anchor time
  expect_no_error(data_bsk_impute)
  expect_no_warning(data_bsk_impute)
  expect_type(data_bsk_impute, "list")

  data_names <- c(".source", "baseline", colnames(epic_train))
  expect_named(data_bsk_impute, data_names, ignore.order = TRUE)
  expect_output(str(data_bsk_impute), "$ baseline", fixed = TRUE)
  expect_output(str(data_bsk_impute),
                paste0(length(data_names), " variables"))
  expect_output(str(data_bsk_impute),
                paste0(length(unique(epic_train$id)) * length(anchor_time)), " obs")
})


test_that("impute of brokenstick anchor2", {
  library(plmlmm)
  library(tictoc)

  data("epic_train")
  data("epic_test")
  anchor_time <- c(5, 6, 7, 8, 9, 10, 12)

  tic()
  data_bsk_impute <- impute_brokenstick(outcome_var = "ht",
                                        time_var = "time",
                                        id_var = "id",
                                        bs_knots = c(5, 10, 15),
                                        anchor_time = anchor_time,
                                        data = epic_train)
  toc()
  ## 10.244 sec elapsed for 4 anchor time
  ## 10.361 sec elapsed for 6 anchor time
  ## 10.369 sec elapsed for 8 anchor time
  expect_no_error(data_bsk_impute)
  expect_no_warning(data_bsk_impute)
  expect_type(data_bsk_impute, "list")

  data_names <- c(".source", "baseline", colnames(epic_train))
  expect_named(data_bsk_impute, data_names, ignore.order = TRUE)
  expect_output(str(data_bsk_impute), "$ baseline", fixed = TRUE)
  expect_output(str(data_bsk_impute),
                paste0(length(data_names), " variables"))
  expect_output(str(data_bsk_impute),
                paste0(length(unique(epic_train$id)) * length(anchor_time)), " obs")
})

#
#
# ## the linear for all of them
# linear <- linear_impute(lm_formula = "ht ~ as.factor(time) * sex + ethnic + genotype + baseline",
#                         data_impute = data_bsk_impute,
#                         data_test = test_baseline,
#                         id_var = "id",
#                         outcome_var = "ht",
#                         time = "time",
#                         anchor_time = anchor_time)
#
# View(linear$training)
# View(linear$testing)
