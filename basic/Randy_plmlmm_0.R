## packages upload -------------------------------------------------------------
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(purrr)
library(splines)
library(tibble)
library(nlme)
library(broom.mixed)
library(gamlss)
library(JMbayes)


# the setting up for the condor server -----------------------------------------
args = commandArgs(trailingOnly = FALSE)
iternum = as.numeric(args[[1]]) + 100

set.seed(iternum)
load("randy_all_data.Rdata")

# functions used in the project ------------------------------------------------
## 0.1 norm L2
norm2 <- function(v) {
  sqrt(sum(v^2))
}
## 0.2 soft
soft_fun <- function(M, v) {
  soft <- M
  ## number of cols
  p <- dim(M)[2]
  for (j in 1 : p) {
    M.j <- M[, j]
    temp.j <- abs(M.j) - v
    temp.j[temp.j < 0] <- 0
    soft[, j] <- sign(M.j) * temp.j
  }
  return(soft)
}

## 0.3 not all na
not_all_na <- function(x) {
  any(!is.na(x))
}

## 0.4 not any na
not_any_na <- function(x) {
  all(!is.na(x))
}

## 0.5 not in
`%!in%` <- Negate(`%in%`)

## 0.6 mahalanobis p
mahalanobis_p <- function(Dmatrix,
                          alpha) {

  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    ## Mahalanobis distance using the chisq pvalues
    as.matrix() %>%
    t()

  matching <<- mahalanobis(df, colMeans(df), cov(df)) %>%
    # mahalanobis(colMeans(.), cov(.)) %>%
    as.data.frame() %>%
    mutate(pvalue = pchisq(., df = def, lower.tail = FALSE)) %>%
    filter(pvalue >= alpha) %>%
    dplyr::select(malahanobis = 1, pvalue = 2) %>%
    rownames_to_column("id")
  # arrange(diff) %>%

  # slice(1:match_num) %>%
  # inner_join(obs_data, by = "id")

  return(matching)
}

## 0.7 mahalanobis_n
mahalanobis_n <- function(Dmatrix,
                          match_num) {
  matching <<- Dmatrix %>%
    as.matrix() %>%
    t() %>%
    mahalanobis(colMeans(.), cov(.)) %>%
    # mahalanobis(colMeans(.), cov(.)) %>%
    ## Now it is a vector of Mahalanobis distance
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(diff) %>%

    slice(1:match_num)
  # inner_join(dataset, by = "id")

  return(matching)
}

## 0.8 euclidean_n
euclidean_n <- function(Dmatrix,
                        match_num) {
  matching <<- Dmatrix %>%
    apply(2, norm, type = "2") %>%
    ## using Frobenius norm
    # apply(lb_sub, 2, norm, type = "f") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(diff) %>%
    slice(1:match_num)

  return(matching)
}

## 0.9 singletime_n
singletime_n <- function(Dmatrix,
                         match_time,
                         match_num) {
  matching <<- Dmatrix %>%
    filter(as.numeric(rownames(.)) == match_time) %>%
    t() %>%
    ## using Frobenius norm
    # apply(lb_sub, 2, norm, type = "f") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(abs(diff)) %>%
    slice(1:match_num)

  return(matching)
}


## 1.2 linear fitting
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
## 1.3 prediction matching
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
  subject <- lb_test %>%
    dplyr::filter(id == sbj)

  test_data = test
  sbj = "103104"
  ind_time <- test_data %>%
    dplyr::filter(id == sbj)
  View(ind_time)
  View(test)

  ## the matching subset
  lb_sub <- lb_data %>%
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

  centiles_obs <-
    centiles.pred(plm, linetype = "centiles",
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

## 1.4 individual people-like-me matching plot
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

    plot
}

## 2.1 lmm dynamic prediction
lmm_pred <- function(train_data,
                     test_data,
                     baseline) {
  # baseline <- test_baseline
  ctrl <- lmeControl(opt = 'optim')
  fitting <-  lme(ht ~ bs(time, knots = c(10, 12, 15), degree = 3) * sex - 1,
                  random = ~ 1 + bs(time, df = 4, degree = 1, intercept = FALSE)| id,
                  control = ctrl,
                  data = train_data)

  time_vec <- unique(test_data$time)
  lmmpred_90 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.9,
    interval = "prediction",
    seed = 555) %>%
    dplyr::select(id, time,
                  # observed = ht,
                  pred,
                  centile05 = low,
                  centile95 = upp)


  lmmpred_80 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.8,
    interval = "prediction",
    seed = 555) %>%
    dplyr::select(id, time,
                  # observed = ht,
                  pred,
                  centile10 = low,
                  centile90 = upp)

  lmmpred_50 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.5,
    interval = "prediction",
    seed = 555) %>%
    dplyr::select(id, time,
                  # observed = ht,
                  pred,
                  centile25 = low,
                  centile75 = upp)

  lmm <- lmmpred_50 %>%
    full_join(lmmpred_80,
              by = c("id", "time", "pred")) %>%
    full_join(lmmpred_90,
              by = c("id", "time", "pred")) %>%
    # mutate(time = round(time, 2)) %>%
    ## na.omit() %>%
    as.data.frame() %>%
    mutate(id = as.character(id)) %>%
    right_join(test_data) %>%
    dplyr::filter(time != 0) %>%
    mutate(coverage50 = ifelse(ht >= `centile25` & ht <= `centile75`, 1, 0),
           coverage80 = ifelse(ht >= `centile10` & ht <= `centile90`, 1, 0),
           coverage90 = ifelse(ht >= `centile05` & ht <= `centile95`, 1, 0),
           # mse = (actual - `50`)^2,
           # biassq = bias^2,
           # var = mse - bias^2,
           bias = abs(ht - pred))
  return(lmm)
}

## 3.1 generate spline
gen_spline <- function(x, knots, degree, theta) {
  basis <- bs(x = x,
              knots = knots,
              degree = degree,
              intercept = FALSE)

  y.spline <- basis %*% theta
  dt <- data.table::data.table(x = x, y.spline = as.vector(y.spline))

  return(list(dt = dt,
              basis = basis,
              knots = knots))
}

## 3.2 plot spline basis
plot_basis <- function(basisdata) {
  dtbasis <- as.data.table(basisdata$basis)
  dtbasis[, x := seq(0, 1, length.out = .N)]
  dtmelt <- melt(data = dtbasis,
                 id = "x",
                 variable.name = "basis",
                 variable.factor = TRUE)

  ggplot(data = dtmelt,
         aes(x = x, y = value, group = basis)) +
    geom_line(aes(color = basis), size = 1) +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(0, 1),
                       breaks = c(0, basisdata$knots, 1)) +
    theme(panel.grid.minor = element_blank())
}

## 3.3 plot spline
plot_spline <- function(basisdata, points = FALSE) {
  p <- ggplot(data = basisdata$dt)

  if (points) {p <- p + geom_point(aes(x = x, y = y), color = "grey75")}

  p <- p + geom_line(aes(x = x, y = y.spline), color = "red", size = 1) +
    # scale_y_continuous(limits = c(0, 1)) +
    # scale_x_continuous(limits = c(0, 1), breaks = knots) +
    theme(panel.grid.minor = element_blank())

  return(p)
}

## 3.4 generate individual random effect
gen_ind_ranef <- function(subset,
                          knots = c(10, 12, 15),
                          degree = 3,
                          sigma = 1.04,
                          vcov = vcov5) {

  ran_coef <- MASS::mvrnorm(n = 1,
                            mu = rep(0, nrow(vcov)),
                            Sigma = vcov)

  ran_ef <- gen_spline(subset$time,
                       knots = knots,
                       degree = degree,
                       theta = ran_coef)

  res_error <- rnorm(n = nrow(subset), sd = sigma)
  random <- ran_ef$dt + res_error
  return(random)
}

## 3.5 generate all random effect
gen_all_ranef <- function(fullset = margin_mean,
                          id = "id",
                          knots = c(10, 12, 15),
                          degree = 3,
                          sigma = 1.04,
                          vcov = vcov5,
                          seed) {
  set.seed(seed)

  simulation_random <- margin_mean %>%
    group_by(id) %>%
    group_map(~gen_ind_ranef(subset = .x,
                             knots = knots,
                             degree = degree,
                             sigma = sigma,
                             vcov = vcov),
              .keep = TRUE) %>%
    map("y.spline") %>%
    unlist()

  simulation_full <- margin_mean %>%
    mutate(varibility = simulation_random) %>%
    mutate(ht = `.fixed` + varibility)

  return(simulation_full)
}

## 3.6 training and testing
save_dataset <- function(dataset) {
  train_id <- unique(dataset$id) %>%
    sample(913)

  dataset <- as.data.frame(dataset) %>%
    mutate(group = case_when(id %in% train_id ~ "training",
                             id %!in% train_id ~ "testing"))

  return(dataset)

}

# codes for the results --------------------------------------------------------
# ctrl <- lmeControl(opt = 'optim')
# # df 5 = quadratic 2 + knots 3
# fit5 <-  lme(ht ~ bs(time, knots = c(10, 12, 15), degree = 3) * sex - 1,
#              random = ~ 1 + bs(time, df = 5, degree = 2, intercept = FALSE)| id,
#              control = ctrl,
#              data = all)
#
# glance5 <- broom.mixed::glance(fit5) %>%
#   mutate(fixed = "fixed = bs(time, degree = 3, knots = c(10, 12, 15))",
#          random = "random = ~ bs(time, df = 5, degree = 2)")
#
# (tidy5 <- broom.mixed::tidy(fit5))
# augment5 <- broom.mixed::augment(fit5)
#
# margin_mean <- augment5 %>%
#   dplyr::select(id, sex, time, `.fixed`)
#
# (vcov5 <- getVarCov(fit5))
simulation <- gen_all_ranef(seed = 1) %>% save_dataset()

## people like me method -----------------------------------------------------
train_data <- dplyr::filter(simulation, group == "training")
test_data <- dplyr::filter(simulation, group == "testing")

train_baseline <-
  train_data %>%
  group_by(id) %>%
  slice(1L)
test_baseline <-
  test_data %>%
  group_by(id) %>%
  slice(1L)

id_train <- unique(train_data$id)
id_test <- unique(test_data$id)

## setting up knots and time ---------------------------------------------------
knots = c(5, 10, 12)
pred_time = c(2, 4, 6, 8, 10, 12, 14)

bks <- brokenstick::brokenstick(ht ~ time | id,
                                data = train_data,
                                knots = knots)

dataset_baseline <- train_data %>%
      group_by(id) %>%
      slice(1L) %>%
      dplyr::select(-time) %>%
      dplyr::select(baseline = ht, everything())

bks_pred_knots <- predict(bks,
                          x = pred_time,
                          group = train_data$id)
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
                          group = test_baseline$id,
                          ids = test_baseline$id)

test_pred <-
  bks_pred_knots %>%
  # pivot_wider(names_from = time,
  #             values_from = .pred) %>%
  select_if(not_all_na) %>%
  # dplyr::select(target = as.character(match_time),
  #               everything()) %>%
  full_join(test_baseline, by = "id") %>%
  dplyr::transmute(.source = .source,
                   id = id,
                   time = time.x,
                   .pred = .pred,
                   time = time.x,
                   baseline = ht,
                   sex = sex,
                   .fixed = .fixed,
                   varibility = varibility,
                   group = group,
                   timef = paste0("time", time.x))

lb_train <-
  linear_brokenstick(
    lm_formula = "`.pred` ~ timef * sex + baseline",
    bks_pred = train_pred)

lb_test <-
  linear_brokenstick(
    lm_formula = "`.pred` ~ timef * sex + baseline",
    bks_pred = test_pred)

train_eld_n10 <- map(id_train,
               ~pred_matching(
                 lb_data = lb_train,
                 lb_test = lb_train,
                 train_data = train_data,
                 test_data = train_data,
                 match_methods = "euclidean",
                 match_num = 10,
                 gamlss_formula = "ht ~ cs(time, df = 3)",
                 gamsigma_formula = "~ cs(time, df = 1)",
                 match_plot = FALSE,
                 predict_plot = FALSE,
                 sbj = .))
train_mhl_p09 <- map(id_train,
               ~try(pred_matching(
                 lb_data = lb_train,
                 lb_test = lb_train,
                 train_data = train_data,
                 test_data = train_data,
                 match_methods = "mahalanobis",
                 match_alpha = 0.9,
                 gamlss_formula = "ht ~ cs(time, df = 3)",
                 gamsigma_formula = "~ cs(time, df = 1)",
                 match_plot = FALSE,
                 predict_plot = FALSE,
                 sbj = .)))

train_mhl_n10 <-   map(id_train,
                 ~pred_matching(
                   lb_data = lb_train,
                   lb_test = lb_train,
                   train_data = train_data,
                   test_data = train_data,
                   match_methods = "mahalanobis",
                   match_num = 10,
                   gamlss_formula = "ht ~ cs(time, df = 3)",
                   gamsigma_formula = "~ cs(time, df = 1)",
                   match_plot = FALSE,
                   predict_plot = FALSE,
                   sbj = .))

train_sgl10_n10 <-   map(id_train,
                   ~pred_matching(
                     lb_data = lb_train,
                     lb_test = lb_train,
                     train_data = train_data,
                     test_data = train_data,
                     match_methods = "single",
                     match_num = 10,
                     match_time = 10,
                     match_alpha = NULL,
                     gamlss_formula = "ht ~ cs(time, df = 3)",
                     gamsigma_formula = "~ cs(time, df = 1)",
                     match_plot = FALSE,
                     predict_plot = FALSE,
                     sbj = .))

# testing --------------------------------------------------------------------


test_mhl_n10 <- map(id_test,
                    ~pred_matching(
                      lb_data = lb_train,
                      lb_test = lb_test,
                      test_data = test_data,
                      train_data = train_data,
                      match_methods = "mahalanobis",
                      match_num = 10,
                      gamlss_formula = "ht ~ cs(time, df = 3)",
                      gamsigma_formula = "~ cs(time, df = 1)",
                      match_plot = FALSE,
                      predict_plot = FALSE,
                      sbj = .))

test_mhl_p09 <- map(id_test,
                   ~try(pred_matching(
                     lb_data = lb_train,
                     lb_test = lb_test,
                     test_data = test_data,
                     train_data = train_data,
                     match_methods = "mahalanobis",
                     match_alpha = 0.90,
                     gamlss_formula = "ht ~ cs(time, df = 3)",
                     gamsigma_formula = "~ cs(time, df = 1)",
                     match_plot = FALSE,
                     predict_plot = FALSE,
                     sbj = .)))


test_sgl10_n10 <- map(id_test,
                 ~pred_matching(
                   lb_data = lb_train,
                   lb_test = lb_test,
                   test_data = test_data,
                   train_data = train_data,
                   match_methods = "single",
                   match_num = 10,
                   match_time = 10,
                   match_alpha = NULL,
                   gamlss_formula = "ht ~ cs(time, df = 3)",
                   gamsigma_formula = "~ cs(time, df = 1)",
                   match_plot = FALSE,
                   predict_plot = FALSE,
                   sbj = .))

## linear mixed model ---------------------------------------------------------
timea <- Sys.time()
lmm_test <- lmm_pred(train_data, test_data, test_baseline)
timeb <- Sys.time()
print("7. lmm works ---------------------------------")
print(timeb - timea)


save(iternum,
     train_eld_n10, train_mhl_p09,
     train_mhl_n10, train_sgl10_n10,
     test_eld_n10, test_mhl_p09,
     test_mhl_n10, test_sgl10_n10,
     # lmm_test,
     file = "randy_simulation_result_20230309.Rdata")



