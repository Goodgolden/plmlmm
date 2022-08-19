# 1 simulation for 1000 datasets -----------------------------------------------
library(tidyverse)
library(splines)
library(MASS)
library(nlme)


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


# simulation
simulation_1k <- map(1:1000,
                     ~gen_all_ranef(seed = .x))
# fitting models to check
estimation_1k <- map(simulation_1k,
                     ~lme(ht ~ bs(time, knots = c(10, 12, 15), degree = 3) * sex - 1,
                          random = ~ 1 + bs(time, df = 5,
                                            degree = 2
                                            intercept = FALSE)| id,
                          control = ctrl,
                          data = .x))


tidy_1k %>% map(estimation_1k, ~broom.mixed::tidy())

glance_1k %>% map(estimation_1k, ~broom.mixed::glance())

save(tidy_1k, glance_1k, file = "simulation_tidy_glance_20220818.Rdata")
