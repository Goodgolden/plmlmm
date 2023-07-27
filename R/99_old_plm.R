## 1.1 brokenstick prediction {{{-----------------------------------------------
#' Title Broken stick model
#' @param outcome
#' @param time
#' @param id
#' @param knots
#' @param pred_time
#' @param choice
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
#'

brokenstick_prediction <-
  function(outcome = "ht",
           time = "time",
           id = "id",
           knots = c(6, 10, 12),
           pred_time = c(6, 8, 10, 12),
           choice = c("predicted", "baseline", "observed", "testing"),
           train_data,
           newdata = NULL) {

  formula <- paste0(outcome, "~", time, "|", id)
  bks <- brokenstick::brokenstick(formula = as.formula(formula),
                                  data = train_data,
                                  knots = knots)
  dataset_baseline <- train_data %>%
      group_by(eval(id)) %>%
      arrange(eval(time)) %>%
      slice(1L) %>%
      dplyr::select(-eval(time), -eval(id)) %>%
      dplyr::select(baseline = eval(outcome), everything())

  if (choice %!in% c("observed", "predicted", "baseline", "testing")) {
    stop("please select one prediction choice from c('all', 'knots', 'baseline')!")
  }

  ### bks_pred
  if (choice == "testing" & is.null(newdata)) {
    stop("please provide testing dataset baseline values.")
  }

  if (choice == "testing") {
    bks_pred_knots <- predict(bks,
                              newdata = newdata,
                              x = pred_time,
                              group = newdata$id,
                              ids = newdata$id)

    dataset_knots <-
      bks_pred_knots %>%
      select_if(not_all_na) %>%
      full_join(test_baseline) %>%
      dplyr::select(baseline = ht, everything())
    # browser()
    return(dataset_knots)
  }


  if (choice == "observed") {
    bks_pred_all <- predict(bks,
                            group = train_data$id)

    dataset_all_long <- full_join(train_data, bks_pred_all) %>%
      dplyr::rename(ht_bks_pred = `.pred`) %>%
      dplyr::select(eval(time), ht_bks_pred,
                    eval(id), eval(outcome)) %>%
      split(f = .$id,
            drop = TRUE) %>%
      map(~ dplyr::select(., eval(time),
                          ht_bks_pred,
                          eval(outcome)) %>%
            remove_rownames())

    return(dataset_all_long)
  }

  if (choice == "predicted") {
    bks_pred_knots <- predict(bks,
                            x = pred_time,
                            group = train_data$id)
    dataset_knots <-
      bks_pred_knots %>%
      select_if(not_all_na) %>%
      # dplyr::select(target = as.character(match_time),
      #               everything()) %>%
      full_join(dataset_baseline, by = c("id" = "eval(id)", "sex")) %>%
      mutate(timef = paste0("time", time))
      # pivot_wider(names_from = time,
      #             values_from = .pred)

    return(dataset_knots)
  }

  if (choice == "baseline") {
    return(dataset_baseline)
    }
  }

## }}}--------------------------------------------------------------------------

## 1.2 linear fitting {{{-------------------------------------------------------
#' Title multivariate linear model
#'
#' @param lm_formula
#' @param bks_pred
#'
#' @return
#' @export

linear_brokenstick <-
  function(lm_formula = "`.pred` ~ time * sex + baseline",
           bks_pred) {

    # bks_pred = test_pred
    lm_bks <- lm(as.formula(lm_formula),
                data = bks_pred)

    predicted <- predict(lm_bks, newdata = bks_pred)

    lb_data <- bks_pred %>%
      ungroup() %>%
      mutate(lm_bks_target = predicted) %>%
      dplyr::select(lm_bks_target) %>%
      cbind(bks_pred) %>%
      dplyr::select("id", time, contains("lm_bks_target")) %>%
      as.matrix() %>%
      as.data.frame() %>%
      mutate(lm_bks_target = as.numeric(lm_bks_target))

    attr(lb_data, "lm_summary") <- summary(lm_bks)

    return(lb_data)
  }
## }}}--------------------------------------------------------------------------






## 1.3 prediction matching {{{--------------------------------------------------
#' Title prediction matching
#'
#' @param lb_data
#' @param obs_data
#' @param match_methods
#' @param match_num
#' @param match_alpha
#' @param match_time
#' @param gamlss_formula
#' @param gamsigma_formula
#' @param match_plot
#' @param predict_plot
#' @param sbj
#'
#' @return
#' @export

pred_matching <- function(lb_data,
                          lb_test,
                          train_data,
                          test_data,
                          match_methods = c("mahalanobis", "euclidean", "single"),
                          match_num = NULL,
                          match_alpha = NULL,
                          match_time = NULL,
                          gamlss_formula = "ht ~ cs(time, df = 3)",
                          gamsigma_formula = "~ cs(time, df = 1)",
                          match_plot = FALSE,
                          predict_plot = FALSE,
                          sbj) {

  if (is.null(match_num) & is.null(match_alpha)) {
    stop("provide matching number or critical values for PLM methods")
  }

  if (!is.null(match_num) & !is.null(match_alpha)) {
    stop("provide either matching number or critical values for PLM methods, not both")
  }


  browser()
  subject <- lb_test

  ind_time <- test_data %>%
    dplyr::filter(id == sbj)

  ## the matching subset
  lb_sub <- lb_data %>%
    transmute(id = as.character(id),
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

  # Sun Aug 28 00:11:37 2022 ------------------------------

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

  if (match_plot == TRUE) {
    matching_plot <- ggplot(matching) +
      geom_line(aes(x = time, y = ht,
                    group = id),
                color = "grey",
                linetype = "dashed") +
      geom_line(data = ind_time,
                aes(x = time, y = ht),
                color = "darkblue",
                size = 1) +
      ggtitle(sbj) +
      xlim(0, 17) +
      ylim(50, 250) +
      theme_bw()

    cat("\n plotting matching paired individual trajectories \n")
  } else {
    matching_plot = NULL
  }


  ## fitting gamlss model for
  plm <- gamlss::gamlss(as.formula(gamlss_formula),
                sigma.formula = as.formula(gamsigma_formula),
                # nu.formula = ~cs(time^0.1, df=1),
                # tau.formula = ~cs(time^0.5, df=1),
                data = matching,
                family = NO)

  cat("\n gamlss model fitting is done \n")

  # Wed Aug  3 14:43:22 2022 ------------------------------

  centiles_obs <-
    centiles.pred(plm, linetype = "centiles",
                  xname = "time",
                  xvalues = c(ind_time$time),
                  cen = c(2.5, 10, 25, 50, 75, 90, 97.5)) %>%
    cbind(actual = ind_time$ht) %>%
    as.data.frame() %>%
    ## ================== defintion of bias and mse =====================
    ## confidence interval is variance or variability
    ## bias is the accuracy ????
    mutate(coverage50 = ifelse(actual >= `25` & actual <= `75`, 1, 0),
           coverage80 = ifelse(actual >= `10` & actual <= `90`, 1, 0),
           coverage95 = ifelse(actual >= `2.5` & actual <= `97.5`, 1, 0),
           # mse = (actual - `50`)^2,
           # biassq = bias^2,
           # var = mse - bias^2,
           bias = abs(actual - `50`))

  cat("\n gamlss model prediction for observed time points are done \n")

   centiles_pred <-
     centiles.pred(plm, linetype = c("centiles"),
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
                  observation = ind_time,
                  title = unique(ind_time$id))
   } else {
     plm_plot <- NULL
   }


   return(list(centiles_observed = centiles_obs,
               centiles_predicted = centiles_pred,
               matching_trajectory = matching_plot,
               predictive_centiles = plm_plot))
}


## }}}--------------------------------------------------------------------------


## 1.4 individual people-like-me matching plot {{{------------------------------
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
## }}}--------------------------------------------------------------------------





#' Title
#'
#' @param outcome
#' @param time
#' @param id
#' @param train_data
#' @param test_data
#' @param knots
#' @param pred_time
#' @param linear_model
#' @param match_num
#' @param match_time
#' @param match_methods
#' @param match_alpha
#' @param gamlss_formula
#' @param gamsigma_formula
#' @param match_plot
#' @param predict_plot
#' @param sbj
#'
#' @return
#' @export
#'
#' @examples
plm <- function(outcome = "ht",
                time = "time",
                id = "id",
                train_data = train,
                test_data = test,
                knots = c(5, 10),
                pred_time = c(4, 6, 8, 12),
                # newdata = test_baseline,
                linear_model = "`.pred` ~ time * sex + baseline",
                match_num = NULL,
                match_time = NULL,
                match_methods = "mahalanobis",
                match_alpha = 0.95,
                gamlss_formula = "ht ~ cs(time, df = 3)",
                gamsigma_formula = "~ cs(time, df = 1)",
                match_plot = TRUE,
                predict_plot = TRUE,
                sbj = sbj){
  ## brokenstick ----------------------------------
  train_baseline <-
    train_data %>%
    group_by(id) %>%
    slice(1L)
  test_baseline <-
    test_data %>%
    group_by(id) %>%
    slice(1L)

  bks <- brokenstick::brokenstick(ht ~ time | id,
                                  data = train_data,
                                  knots = knots)

  dataset_baseline <- train_data %>%
    group_by(id) %>%
    slice(1L) %>%
    dplyr::select(-time) %>%
    dplyr::select(baseline = ht, everything())

  id_train <- unique(train_data$id)
  id_test <- unique(test_data$id)

  bks_pred_knots <- predict(bks,
                            x = pred_time,
                            shape = "long",
                            # group = train_data$id,
                            include_data = FALSE) %>%
    dplyr::select(id, time, `.pred`)

  train_pred <-
    bks_pred_knots %>%
    # pivot_wider(names_from = time,
    #             values_from = .pred) %>%
    select_if(not_all_na) %>%
    # dplyr::select(target = as.character(match_time),
    #               everything()) %>%
    full_join(dataset_baseline, by = c("id")) %>%
    mutate(timef = paste0("time", time))


  bks_pred_knots <- predict(bks,
                            newdata = test_baseline,
                            x = pred_time,
                            group = test_baseline$id) %>%
    dplyr::select(id, time, .pred) %>%
    filter(time != 0)

  test_pred <-
    bks_pred_knots %>%
    # pivot_wider(names_from = time,
    #             values_from = .pred) %>%
    select_if(not_all_na) %>%
    # dplyr::select(target = as.character(match_time),
    #               everything()) %>%
    full_join(test_baseline, by = "id") %>%
    mutate(time = time.x,
           timef = paste0("time", time.x)) %>%
    dplyr::select(-time.x, -time.y) %>%
    dplyr::select(baseline = ht, everything())

  ## linear model ---------------------------------
  lb_train <-
    linear_brokenstick(
      lm_formula = linear_model,
      bks_pred = train_pred)


  lb_test <-
    linear_brokenstick(
      lm_formula = linear_model,
      bks_pred = test_pred)

  ## plm model --------------------------
  plm_model <-
    pred_matching(
      lb_data = lb_train,
      lb_test = lb_test,
      train_data = train_data,
      test_data = test_data,
      match_methods = match_methods,
      match_alpha = match_alpha,
      match_num = match_num,
      match_time = match_tiem,
      gamlss_formula = gamlss_formula,
      gamsigma_formula = gamsigma_formula,
      match_plot = match_plot,
      predict_plot = match_plot,
      sbj = sbj)

  return(plm_model)
}



