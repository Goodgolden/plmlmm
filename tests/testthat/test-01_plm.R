test_that("brokenstick prediction", {
  bks_pred <- brokenstick_prediction(dataset = train,
                                     knots = c(6, 10, 12),
                                     pred_time = c(2, 4, 6, 8, 10, 12),
                                     choice = "predicted")


  # Wed Jul 27 10:42:35 2022 ------------------------------
  # bks_pred1 <- brokenstick_prediction(dataset = train,
  #                                    pred_time = c(2, 4),
  #                                    choice = "predicted")
  # # Wed Aug  3 12:59:29 2022 ------------------------------
  # bks_pred2 <- brokenstick_prediction(dataset = train,
  #                                     knots = c(2, 4, 6, 8 ,10),
  #                                     pred_time = c(2, 4, 6, 8, 10),
  #                                     choice = "predicted")
  #
  # bks_base1 <- brokenstick_prediction(dataset = test,
  #                                        choice = "baseline")

  expect_output(str(bks_pred), "13 variable")

  lb_data <- linear_brokenstick(lm_formula = "`.pred` ~ time * sex + baseline",
                                bks_pred = bks_pred)

  expect_success(expect_type(lb_data, 'list'))
  expect_output(str(lb_data), "3 variable")
  expect_named(lb_data, c('id', 'time', 'lm_bks_target'),
               ignore.order = TRUE,
               ignore.case = TRUE)
  # expect_snapshot_output(head(lb_data, 5))

  pm_mhl_p08_103125 <- pred_matching(lb_data = lb_data,
                                      obs_data = train,
                                      match_methods = "mahalanobis",
                                      match_alpha = 0.8,
                                      gamlss_formula = "ht ~ cs(time, df = 3)",
                                      gamsigma_formula = "~ cs(time, df = 1)",
                                      # match_plot = TRUE,
                                      predict_plot = TRUE,
                                      sbj = 103125)

  pm_mhl_n20_103125 <- pred_matching(lb_data = lb_data,
                                     obs_data = train,
                                     match_methods = "mahalanobis",
                                     match_num = 20,
                                     gamlss_formula = "ht ~ cs(time, df = 3)",
                                     gamsigma_formula = "~ cs(time, df = 1)",
                                     # match_plot = TRUE,
                                     predict_plot = TRUE,
                                     sbj = 103125)

  pm_eld_n20_103125 <- pred_matching(lb_data = lb_data,
                                     obs_data = train,
                                     match_methods = "euclidean",
                                     match_num = 20,
                                     gamlss_formula = "ht ~ cs(time, df = 3)",
                                     gamsigma_formula = "~ cs(time, df = 1)",
                                     # match_plot = TRUE,
                                     predict_plot = TRUE,
                                     sbj = 103125)

  pm_sgl10_n20_159633 <- pred_matching(lb_data = lb_data,
                                     obs_data = train,
                                     match_methods = "single",
                                     match_num = 20,
                                     match_time = 10,
                                     match_alpha = NULL,
                                     gamlss_formula = "ht ~ cs(time, df = 3)",
                                     gamsigma_formula = "~ cs(time, df = 1)",
                                     match_plot = TRUE,
                                     predict_plot = TRUE,
                                     sbj = 159633)

  expect_named(pm_mhl_p08_103125,
               c('centiles_observed', 'centiles_predicted',
                 'matching_trajectory', 'predictive_centiles'),
               ignore.order = TRUE,
               ignore.case = TRUE)

  expect_named(pm_mhl_n20_103125,
               c('centiles_observed', 'centiles_predicted',
                 'matching_trajectory', 'predictive_centiles'),
               ignore.order = TRUE,
               ignore.case = TRUE)

  expect_named(pm_eld_n20_103125,
               c('centiles_observed', 'centiles_predicted',
                 'matching_trajectory', 'predictive_centiles'),
               ignore.order = TRUE,
               ignore.case = TRUE)

  expect_named(pm_sgl10_n20_159633,
               c('centiles_observed', 'centiles_predicted',
                 'matching_trajectory', 'predictive_centiles'),
               ignore.order = TRUE,
               ignore.case = TRUE)

  id_all_10 <- unique(train$id) %>% head(10)

})



# test_that("pred_matching", {
# pm_mhl_p0.8_103125 <- pred_matching(lb_data = lb_data,
#                           obs_data = train,
#                           match_methods = "mahalanobis",
#                           match_alpha = 0.8,
#                           gamlss_formula = "ht ~ cs(time, df = 3)",
#                           gamsigma_formula = "~ cs(time, df = 1)",
#                           match_plot = TRUE,
#                           predict_plot = TRUE,
#                           sbj = 103125)
#
# pm_mhl_n20_103125 <- pred_matching(lb_data = lb_data,
#                                     obs_data = train,
#                                     match_methods = "mahalanobis",
#                                     match_num = 20,
#                                    match_alpha = NULL,
#                                     gamlss_formula = "ht ~ cs(time, df = 3)",
#                                     gamsigma_formula = "~ cs(time, df = 1)",
#                                     match_plot = TRUE,
#                                     predict_plot = TRUE,
#                                     sbj = 103125)
#
# pm_sgl_n20_103125 <- pred_matching(lb_data = lb_data,
#                                    obs_data = train,
#                                    match_methods = "single",
#                                    match_num = 20,
#                                    match_time = 10,
#                                    match_alpha = NULL,
#                                    gamlss_formula = "ht ~ cs(time, df = 3)",
#                                    gamsigma_formula = "~ cs(time, df = 1)",
#                                    match_plot = TRUE,
#                                    predict_plot = TRUE,
#                                    sbj = 103125)
# })


# test_that("linear_brokenstcik", {
# lb_data <- linear_brokenstick(lm_formula = "`.pred` ~ time * sex + baseline",
#                               bks_pred = bks_pred)
# # attr(lb_data, "lm_summary") %>% broom.mixed::glance()
# })
