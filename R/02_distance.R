

## 2.1 distance_df -----------------------------------------------------------------
#' Title Distance calculation
#'
#' @param lb_train
#' @param lb_test_ind
#' @param match_methods
#' @param match_time
#' @param id_var
#' @param outcome_var
#' @param time_var
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
distance_df <- function(lb_train,
                        lb_test_ind,
                        match_methods = c("euclidean", "mahalanobis", "single"),
                        match_time = NULL,
                        id_var,
                        outcome_var,
                        time_var,
                        ...) {
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  ## the matching subset
  lb_sub1 <- lb_train %>%
    pivot_wider(names_from = {{ id_var }},
                values_from = {{ outcome_var }}) %>%
    column_to_rownames(var = as.character({{ time_var }})) %>%
    mutate_all(as.numeric)

  center = as.numeric(unlist(lb_test_ind[, 3]))

  if (match_methods == "euclidean") {
    dist_df <<- euclidean_df(Dmatrix = lb_sub1,
                             center = center)
    cat("\n using euclidean distance\n")
  }

  if (match_methods == "mahalanobis") {
      dist_df <<- mahalanobis_df(Dmatrix = lb_sub1,
                                 center = center)
      cat("\n using mahalanobis distance\n")
  }

  # if (match_methods == "single") {
  #   if (is.null(match_time)) {
  #     stop("provide matching time points for single-time PLM methods")
  #   }
  #   dist_df <<- single_df(Dmatrix = lb_sub2,
  #                              match_time = match_time)
  #   cat("\n using single critical time point matching \n")
  # }

  return(distance = dist_df)
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

## 2.2 matching ----------------------------------------------------------------

#' Title Finding the matches subset
#'
#' @param distance_df
#' @param train
#' @param test_one
#' @param id_var
#' @param outcome_var
#' @param time_var
#' @param match_alpha
#' @param match_number
#' @param match_plot
#'
#' @return
#' @export
#'
#' @examples
match <- function(distance_df = ddd,
                  train = train,
                  test_one,
                  id_var = "id",
                  outcome_var = "ht",
                  time_var = "time",
                  match_alpha = NULL,
                  match_number = NULL,
                  match_plot = FALSE) {

  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)


  if (is.null(match_alpha)) {
    data <- distance_df %>%
      slice(1:match_num) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (is.null(match_number)) {
    data <- distance_df %>%
      filter(pvalue >= match_alpha) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (match_plot == TRUE) {

    matching_plot <- ggplot() +
      geom_line(data = data, aes(x = {{ time_var }}, y = {{ outcome_var }},
                    group = {{ id_var }}),
                color = "grey",
                linetype = "dashed") +
      geom_line(data = test_one,
                aes(x = {{time_var}}, y = {{outcome_var}}),
                color = "darkblue",
                linewidth = 1) +
      theme_bw()

    cat("\n plotting matching paired individual trajectories \n")
  } else {
    matching_plot = NULL
  }

  return(list(subset = data,
              plot = matching_plot,
              id = unique(test_one[[as_label(enquo(id_var))]]),
              alpha = match_alpha,
              number = match_number))
}

## 2.3 people like me prediction -----------------------------------------------
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
                          id_var = "id",
                          time_var = "time",
                          outcome_var = "ht",
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

## 2.4 individual people-like-me matching plot ---------------------------------
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

