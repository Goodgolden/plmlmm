## 3.1 generate spline {{{------------------------------------------------------
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

## }}}--------------------------------------------------------------------------



## 3.2 plot spline basis {{{----------------------------------------------------
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
## }}}--------------------------------------------------------------------------




## 3.3 plot spline {{{----------------------------------------------------------
plot_spline <- function(basisdata, points = FALSE) {
  p <- ggplot(data = basisdata$dt)

  if (points) {p <- p + geom_point(aes(x = x, y = y), color = "grey75")}

  p <- p + geom_line(aes(x = x, y = y.spline), color = "red", size = 1) +
    # scale_y_continuous(limits = c(0, 1)) +
    # scale_x_continuous(limits = c(0, 1), breaks = knots) +
    theme(panel.grid.minor = element_blank())

  return(p)
}
## }}}--------------------------------------------------------------------------


## 3.4 generate individual random effect {{{------------------------------------
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

## }}}--------------------------------------------------------------------------

## 3.5 generate all random effect {{{-------------------------------------------
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
## }}}--------------------------------------------------------------------------


# simulation1 <- gen_all_ranef(seed = 6)
# View(simulation1)























