## 3.1 people like me prediction -----------------------------------------------
#' Title Prediction with GAMLSS model
#'
#' @param matching
#' @param test_one
#' @param id_var
#' @param time_var
#' @param outcome_var
#' @param gamlss_formula
#' @param gamsigma_formula
#' @param weight
#' @param predict_plot
#'
#' @return
#' @export
#'
#' @examples
predict_gamlss <- function(matching,
                           test_one,
                           id_var,
                           time_var,
                           outcome_var,
                           gamlss_formula = "ht ~ cs(time, df = 3)",
                           gamsigma_formula = "~ cs(time, df = 1)",
                           weight = FALSE,
                           predict_plot = FALSE) {


  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  matching2 <<- matching %>% dplyr::select(-diff, -pvalue)
  # test_one[[as.character({{ time_var }})]]

  if (weight == FALSE) {
    w = NULL
  } else {
    w = matching$pvalue
  }

  plm <- gamlss::gamlss(as.formula(gamlss_formula),
                        sigma.formula = as.formula(gamsigma_formula),
                        # nu.formula = ~cs(time^0.1, df=1),
                        # tau.formula = ~cs(time^0.5, df=1),
                        weights = w,
                        method = RS(100),
                        data = matching2,
                        family = NO)

  centiles_obs <-  gamlss::centiles.pred(plm,
                                         type = c("centiles"),
                                         xname = as.character({{time_var}}),
                                         xvalues = test_one$time ,
                                         cen = c(5, 10, 25, 50, 75, 90, 95)) %>%
    cbind(actual = test_one[[as.character({{ outcome_var }})]]) %>%
    as.data.frame() %>%
    mutate(coverage50 = ifelse(actual >= `C25` & actual <= `C75`, 1, 0),
           coverage80 = ifelse(actual >= `C10` & actual <= `C90`, 1, 0),
           coverage90 = ifelse(actual >= `C5` & actual <= `C95`, 1, 0),
           # mse = (actual - `50`)^2,
           # biassq = bias^2,
           # var = mse - bias^2,
           bias = abs(actual - `C50`))

  cat("\n gamlss model prediction for observed time points are done \n")

  centiles_pred <-
    centiles.pred(plm,
                  linetype = c("centiles"),
                  xname = "time",
                  xvalues = c(0:17),
                  cent = c(5, 10, 25, 50, 75, 90, 95),
                  plot = FALSE,
                  legend = T) %>%
    dplyr::select(time = 1,
                  q05 = 2,
                  q10 = 3,
                  q25 = 4,
                  q50 = 5,
                  q75 = 6,
                  q90 = 7,
                  q95 = 8) %>%
    mutate(cfint90 = q95 - q05,
           cfint80 = q90 - q10,
           cfint50 = q75 - q25)

  cat("\n gamlss model prediction for predicted time points are done \n")

  if (predict_plot == TRUE) {
    plm_plot <- plm_ind_plot(quantile = centiles_pred,
                             observation = test_one)
  } else {
    plm_plot <- NULL
  }

  return(list(centiles_observed = centiles_obs,
              centiles_predicted = centiles_pred,
              predictive_centiles = plm_plot))
}

## 3.2 individual people-like-me matching plot ---------------------------------
#' Title plot individual matching
#'
#' @param quantile
#' @param observation
#' @param title
#'
#' @return
#' @export
plm_ind_plot <- function(quantile,
                         observation,
                         title = NULL) {

  plot <- ggplot() +
    geom_line(data = quantile, aes(x = time, y = q05),
              color = "dodgerblue", linetype = "dashed",
              alpha = 0.5) +
    geom_line(data = quantile, aes(x = time, y = q95),
              color = "dodgerblue", linetype = "dashed",
              alpha = 0.5) +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q05, ymax = q95),
                fill = "dodgerblue", alpha = 0.5) +
    geom_line(data = quantile, aes(x = time, y = q10),
              color = "dodgerblue2", linetype = "dashed",
              alpha = 0.7) +
    geom_line(data = quantile, aes(x = time, y = q90),
              color = "dodgerblue2", linetype = "dashed",
              alpha = 0.7) +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q10, ymax = q90),
                fill = "dodgerblue2", alpha = 0.7) +
    geom_line(data = quantile, aes(x = time, y = q25),
              color = "dodgerblue3", linetype = "dashed",
              alpha = 0.8) +
    geom_line(data = quantile, aes(x = time, y = q75),
              color = "dodgerblue3", linetype = "dashed",
              alpha = 0.8) +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q25, ymax = q75),
                fill = "dodgerblue3", alpha = 0.8) +
    geom_line(data = quantile, aes(x = time, y = q50),
              color = "dodgerblue4", linetype = "dashed") +
    geom_point(data = observation, aes(x = time, y = ht),
               color = "black", size = 1) +
    theme_bw() +
    xlab("Time (yr)") +
    ylab("Height (cm)") +
    ggtitle(title) +
    xlim(0, 17) +
    ylim(50, 250)

  # print(range(observation$time))
  plot
}


## 3.3 dis_match_pred ----------------------------------------------------------

predict_matching <- function(lb_data,
                              lb_test,
                              train_data,
                              test_data,
                              match_methods = c("mahalanobis", "euclidean", "single"),
                              match_num = NULL,
                              match_alpha = NULL,
                              match_time = NULL,
                              gamlss_formula = "ht ~ cs(time, df = 3)",
                              gamsigma_formula = "~ cs(time, df = 1)",
                              sbj) {
  if (is.null(match_num) & is.null(match_alpha)) {
    stop("provide matching number or critical values for PLM methods")
  }
  if (!is.null(match_num) & !is.null(match_alpha)) {
    stop("provide either matching number or critical values for PLM methods, not both")
  }

  subject <- lb_test %>%
    mutate(id = as.character(id)) %>%
    dplyr::filter(id == sbj)

  ind_time <- test_data %>%
    mutate(id = as.character(id)) %>%
    dplyr::filter(id == sbj)

  ## the matching subset
  lb_sub <- lb_data %>%
    mutate(id = as.character(id)) %>%
    dplyr::transmute(id = as.character(id),
                     ## more time points for matching
                     ## adding the correlation
                     time = time,
                     diff = lm_bks_target - subject$lm_bks_target) %>%
    ## must remove the self data in training dataset
    dplyr::filter(id != sbj) %>%
    pivot_wider(names_from = "id",
                values_from = "diff") %>%
    remove_rownames() %>%
    column_to_rownames("time")

  if (match_methods == "euclidean") {
    matching <<- euclidean_n(Dmatrix = lb_sub,
                             match_num = match_num) %>%
      inner_join(train_data, by = "id")
    cat("\n using euclidean distance \n")
  }

  if (match_methods == "mahalanobis") {
    if (!is.null(match_num)) {

      matching <<- mahalanobis_n(Dmatrix = lb_sub,
                                 match_num = match_num) %>%
        inner_join(train_data, by = "id")
      cat("\n using mahalanobis distance with matching number \n")}

    if (!is.null(match_alpha)) {
      matching <<- mahalanobis_p(Dmatrix = lb_sub,
                                 alpha = match_alpha) %>%
        inner_join(train_data, by = "id")
      cat("\n using mahalanobis distance with F test p value \n")}
  }

  if (match_methods == "single") {
    matching <<- singletime_n(Dmatrix = lb_sub,
                              match_time = match_time,
                              match_num = match_num) %>%
      inner_join(train_data, by = "id")

    cat("\n using single critical time point matching \n")
  }


  ## fitting gamlss model for
  plm <- gamlss::gamlss(as.formula(gamlss_formula),
                        sigma.formula = as.formula(gamsigma_formula),
                        # nu.formula = ~cs(time^0.1, df=1),
                        # tau.formula = ~cs(time^0.5, df=1),
                        method = RS(100),
                        data = matching,
                        family = NO)

  centiles_obs <-  gamlss::centiles.pred(plm,
                                         type = c("centiles"),
                                         xname = "time",
                                         xvalues = c(ind_time$time),
                                         cen = c(5, 10, 25, 50, 75, 90, 95)) %>%
    cbind(actual = ind_time$ht) %>%
    as.data.frame() %>%
    mutate(coverage50 = ifelse(actual >= `25` & actual <= `75`, 1, 0),
           coverage80 = ifelse(actual >= `10` & actual <= `90`, 1, 0),
           coverage90 = ifelse(actual >= `5` & actual <= `95`, 1, 0),
           # mse = (actual - `50`)^2,
           # biassq = bias^2,
           # var = mse - bias^2,
           bias = abs(actual - `50`))

  # centiles_obs <-
  #   centiles.pred(plm, linetype = "centiles",
  #                 xname = "time",
  #                 xvalues = c(ind_time$time),
  #                 cen = c(5, 10, 25, 50, 75, 90, 95)) %>%
  #   cbind(actual = ind_time$ht) %>%
  #   as.data.frame() %>%
  #   mutate(coverage50 = ifelse(actual >= `25` & actual <= `75`, 1, 0),
  #          coverage80 = ifelse(actual >= `10` & actual <= `90`, 1, 0),
  #          coverage90 = ifelse(actual >= `5` & actual <= `95`, 1, 0),
  #          # mse = (actual - `50`)^2,
  #          # biassq = bias^2,
  #          # var = mse - bias^2,
  #          bias = abs(actual - `50`))


  return(list(centiles_observed = centiles_obs))
}

# all_people <- linear$testing %>%
#   group_by("id") %>%
#   group_map(data.frame) %>%
#   map(~ .x[[as_label(enquo(outcome_var))]]) %>%
#   map(~distance_df(lb_train = linear$training,
#                   lb_test_ind = .,
#                   match_methods = "mahalanobis",
#                   id_var = "id",
#                   time_var = "time",
#                   outcome_var = "ht"))



# test_103104 <- test %>% filter(id == 156392)
#
# plm_individual <- people_like_me(train_data = train,
#                                  test_data = test_103104,
#                                  outcome_var = "ht",
#                                  time_var = "time",
#                                  id_var = "id",
#                                  brokenstick_knots = c(5, 12, 15),
#                                  anchor_time = c(5, 10, 11, 12),
#                                  linear_formula = "ht ~ as.factor(time) * sex + ethnic + genotype + baseline",
#                                  match_methods = "mahalanobis",
#                                  match_alpha = 0.99,
#                                  match_number = NULL,
#                                  weight = FALSE,
#                                  match_plot = TRUE)
#
#
# View(plm_individual)
# View(plm_individual$gamlss_data)
# View(test)
#
# attributes(plm_individual)


