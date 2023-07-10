library(dplyr)
library(magrittr)
library(lme4)
library(jtools)
library(lmerTest)
library(ggplot2)
library(nlme)


#----- Read in data source-------------------------------------------------------------------------------------------------------------------------

dat <- readRDS("Data/tug_dat.rds")
str(dat)



# ---- Create new variables for modelling -----------------------------------------------------------------------------------------------------

dat %<>% mutate(
  # Knot variables for piecewise linear model
  k.30 = as.numeric(if_else(time > 30, time - 30, 0)),
  k.20 = as.numeric(if_else(time > 20, time - 20, 0)),
  k.40 = as.numeric(if_else(time > 40, time - 40, 0)),
  k.50 = as.numeric(if_else(time > 50, time - 50, 0)),
  k.60 = as.numeric(if_else(time > 60, time - 60, 0)),
  k.90 = as.numeric(if_else(time > 90, time - 90, 0)),
  k.120 = as.numeric(if_else(time > 120, time - 120, 0)),

  # id should be factor variable
  id = as.factor(id),


  # Centered time variable to avoid scaling issues
  time.center = as.numeric((time - mean(time)) / sd(time))
)





# ---- Examine demographics and baseline information  ----------------------------------------------------------------------------------------


# total subjects = 644, 1692 observations
# dat %>% group_by(id) %>% slice(1L) %>% nrow()


# observations per patient for entire dataset = 2.62
# 1692 / 644 = 2.62

# Median follow up time = 77 postoperative days  days, IQR = 43 to 191 days
# fu <- dat %>% group_by(id) %>% arrange(desc(time)) %>% slice(1L)
# median(fu$time)
# summary(fu$time)




# ------- Demographics --------------------------------------------------------------------------------------------------------------

# dat.dem <- dat %>% group_by(id) %>% slice(1L)

# Sex distribution (261 males, 381 females)
# summary(dat.dem$sex_f)

# Mean BMI: 31.75 (SD 6.5)
# summary(dat.dem$bmi)
# sd(na.omit(dat.dem$bmi))

# Mean age: 65.9 (SD 8.7)
# summary(dat.dem$age)
# sd(na.omit(dat.dem$age))


# ----- Examine univariate relationships with primary outcome ------------------------------------------------------------------------------

# tug and time (adjusted R2 = 0.028)*
# tug_t <- lm(tug ~ time, data = dat)
# summary(tug_t)

# log(tug) and time (adjusted R2 = 0.047)*
# tug_t.log <- lm(log(tug) ~ time, data = dat)
# summary(tug_t.log)


# bmi and tug (adjusted R2 = 0.012)*
# tug_b <- lm(tug ~ bmi, data = dat)
# summary(tug_b)

# pre_tug and tug (adjusted R2 = 0.14)*
# pre_tug <- lm(tug ~ pre_tug, data = dat)
# summary(pre_tug)

# baseline tug and tug (adjusted R2 = 0.4)*
# base_tug <- lm(tug ~ base_tug, data = dat)
# summary(base_tug)

# sex and tug (adjusted R2 = .03)*
# tug_sex <- lm(tug ~ sex_f, data = dat)
# summary(tug_sex)

# pre_quad_in and tug (adjusted R2= 0.01)*
# tug_quad_pre <- lm(tug ~ pre_quad_in, data = dat)
# summary(tug_quad_pre)

# surgeon and tug (adjusted R2 = 0.005)*
# tug_surg <- lm(tug ~ surgeon, data = dat)
# summary(tug_surg)

# location and tug (adjusted R2 = 0.009)
# tug_loc <- lm(tug ~ location, data = dat)
# summary(tug_loc)

# * = statistically significant

# ------------ Examine relationships between predictors -----------------------

# correlation between baseline tug and bmi (r = 0.12)
# cor(dat$base_tug, dat$bmi, method = "pearson", use = "complete.obs")


# correlation between baseline tug and preop tug (r = 0.37)
# cor(dat$base_tug, dat$pre_tug, method = "pearson", use = "complete.obs")



# correlation between baseline tug and age (r = 0.01)
# cor(dat$base_tug, dat$age, method = "pearson", use = "complete.obs")



# baseline tug and sex (R2 =.036)*
# p1 <- lm(base_tug ~ sex_f, data = dat)
# summary(p1)




# ----------------- Vizualize data ------------------------------------------------------

# Spaghetti

ggplot(data = dat, aes(x = time, y = tug)) +
  geom_point(aes(x = time, y = tug), colour = "black", size = 1) +
  geom_line(aes(group = id, x = time, y = tug), colour = "black", size = 1) +
  coord_cartesian(xlim = c(0, 1000), ylim = c(0, 60)) +
  theme_bw()

# scatter

ggplot(data = dat, aes(x = time, y = tug)) +
  geom_point(aes(x = time, y = tug), colour = "black", size = 1) +
  coord_cartesian(xlim = c(0, 1000), ylim = c(0, 60)) +
  theme_bw()


# ----------------- Build mixed models -------------------------------- ---------------------------------


# Intercept only model
# m0 <- lmer(tug ~ 1 + (1|id), dat = dat, REML = TRUE)
# summary(m0)
# summ(m0)




#------- Quadratic time term ----------------------------------------------------------


# Random intercept and slope
m1 <- lmer(tug ~ 1 + bmi + sex_f + age + time.center + I(time.center^2) + (1 + time.center + I(time.center^2) | id),
  data = dat,
  REML = TRUE
)

# Random effects and residual variance are probably unidentifiable



# Random intercept and slope (don't estimate covariance between random effects)
m1.nocov <- lmer(tug ~ 1 + bmi + sex_f + age + time.center + I(time.center^2) + (1 | id) + (0 + time.center | id) + (0 + I(time.center^2) | id),
  data = dat,
  REML = TRUE
)

summary(m1.nocov)
summ(m1.nocov)
VarCorr(m1.nocov)
vcov(m1.nocov)
plot(m1.nocov)
qqnorm(residuals(m1.nocov)) # residuals don't look normally distributed
hist(residuals(m1.nocov))


# Random intercept and slope without RE covariance (log transformed)

m1.nocov.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time.center + I(time.center^2) + (1 | id) + (0 + time.center | id) + (0 + I(time.center^2) | id),
  data = dat,
  REML = TRUE
)

plot(m1.nocov.log)
qqnorm(residuals(m1.nocov.log)) # Residuals look better. Still not perfect
hist(residuals(m1.nocov.log))
summ(m1.nocov.log)
VarCorr(m1.nocov.log)
vcov(m1.nocov.log)





# Random intercept only
m1.int <- lmer(tug ~ 1 + bmi + sex_f + age + time.center + I(time.center^2) + (1 | id),
  data = dat,
  REML = TRUE
)

ranova(m1.int)
summ(m1.int)
VarCorr(m1.int)
vcov(m1.int)
plot(m1.int)
qqnorm(residuals(m1.int)) # Residuals don't look good



# Random intercept with log transformation
m1.int.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time.center + I(time.center^2) + (1 | id),
  data = dat,
  REML = TRUE
)

plot(m1.int.log)
qqnorm(residuals(m1.int.log))
hist(residuals(m1.int.log)) # Residuals look better


# Compare covariance structure between m1.nocov.log and m1.int.log

logLik(m1.nocov.log) # -360.5717 (df=10)
logLik(m1.int.log) # 'log Lik.' -395.7973 (df=8)

-2 * (-395.7973) - (-2 * -360.5717) # -2loglik = 70.45 (PREFER full model: m1.nocov.log)

# ---------  Linear spline term --------------------------------------------------------------



m2 <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.40 + (1 + time + k.40 | id),
  data = dat,
  REML = TRUE
)

# Random effects are unidentifiable


# m2 without covariance terms for Random effects
m2.nocov <- lmer(tug ~ 1 + bmi + sex_f + age + time + k.40 + (1 | id) + (0 + time | id) + (0 + k.40 | id),
  data = dat,
  REML = TRUE
)

# boundary (singular) fit: see ?isSingular





# Random intercept only
m2.int <- lmer(tug ~ 1 + bmi + sex_f + age + time + k.40 +
                 (1 | id),
               data = dat,
               REML = TRUE)

plot(m2.int)
ranova(m2.int)
summ(m2.int)
VarCorr(m2.int)
vcov(m2.int)
qqnorm(residuals(m2.int)) # Residuals don't appear normally distributed
hist(residuals(m2.int))


# Examine log transformation
m2.int.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + pre_tug + time + k.40 +
                     (1 | id),
                   data = dat,
                   REML = TRUE)
plot(m2.int.log)
ranova(m2.int.log)
summ(m2.int.log)
VarCorr(m2.int.log)
vcov(m2.int.log)
qqnorm(residuals(m2.int.log)) 
# Residuals looks better but not perfect


## linear time only -----------------------------------------------------------
m3 <- lmer(tug ~ 1 + bmi + sex_f + age + time + (1 + time | id),
  data = dat,
  REML = TRUE)


# Warning: Model failed to converge

m3.nocov <- lmer(tug ~ 1 + bmi + sex_f + age + time + (1 | id) + (1 | time),
  data = dat,
  REML = TRUE
)

plot(m3.nocov)
qqnorm(residuals(m3.nocov)) # Residuals don't look good
hist(residuals(m3.nocov))


# Log transform
m3.nocov.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + (1 | id) + (1 | time),
  data = dat,
  REML = TRUE
)

plot(m3.nocov.log)
qqnorm(residuals(m3.nocov.log)) # Residuals look better
hist(residuals(m3.nocov.log))



# ------------ Alternative spline approaches? --------------------------------------------------------------------------------------------






# ----------- Examine m2.log alternative knot placements ---------------------------------------------------------------#

m2.int.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.40 + (1 | id),
  data = dat,
  REML = FALSE
)

m2.log.20 <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.20 + (1 | id),
  data = dat,
  REML = FALSE
)

m2.log.30 <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.30 + (1 | id),
  data = dat,
  REML = FALSE
)


m2.log.50 <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.50 + (1 | id),
  data = dat,
  REML = FALSE
)

m2.log.60 <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.60 + (1 | id),
  data = dat,
  REML = FALSE
)

m2.log.90 <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.90 + (1 | id),
  data = dat,
  REML = FALSE
)

AIC(m2.int.log) #******************
AIC(m2.log.20)
AIC(m2.log.30)
AIC(m2.log.50)
AIC(m2.log.60)
AIC(m2.log.90)


# k.40 spline performs best



#------------ Examine final model performances -----------------------------------------------------------------------------------------------



#------------ Quadratic time performance # ---------------
m1.nocov.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time.center + I(time.center^2) + 
                       (1 | id) + (0 + time.center | id) + (0 + I(time.center^2) | id),
                     data = dat,
                     REML = TRUE)
plot(m1.nocov.log)
hist(residuals(m1.nocov.log))
qqnorm(residuals(m1.nocov.log))


# Plot population mean response over time
dat.plot <- dat %>%
  select(id, bmi, sex_f, age, time.center, tug) %>%
  na.omit()
dat.plot %<>% mutate(yhat = predict(m1.nocov.log, type = "response"))

ggplot(data = dat.plot, aes(x = time.center, y = tug)) +
  geom_point(aes(x = time.center, y = tug)) +
  geom_line(aes(group = id, x = time.center, y = tug), colour = "black", size = 1) +
  geom_smooth(aes(x = time.center, y = exp(yhat)), method = loess) +
  # coord_cartesian(xlim = c(0, 800), ylim = c(0,40))+
  theme_bw()



# calibration plot

# Calibration plot
ggplot(data = dat.plot) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(x = log(tug), y = yhat))



#--------------- Linear spline performance -----------------------------#


m2.int.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.40 + (1 | id),
  data = dat,
  REML = TRUE
)

plot(m1.int.log)
hist(residuals(m1.int.log))
qqnorm(residuals(m1.int.log))


dat.plot <- dat %>%
  select(id, sex_f, bmi, age, time, k.40, tug) %>%
  na.omit()


dat.plot %<>% mutate(yhat.2 = predict(m2.int.log, type = "response"))

# Plot mean response over time
ggplot(data = dat.plot, aes(x = time, y = tug)) +
  geom_point(aes(x = time, y = tug)) +
  geom_line(aes(group = id, x = time, y = tug), colour = "black", size = 1) +
  geom_smooth(aes(x = time, y = exp(yhat.2)), method = loess) +
  # coord_cartesian(xlim = c(0, 800), ylim = c(0,40))+
  theme_bw()


# Calibration plot
ggplot(data = dat.plot) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(x = log(tug), y = yhat.2))




# -----------------Linear time only performance --------------------------------
m3.nocov.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + 
                       (1 | id) + (1 | time),
                     data = dat,
                     REML = TRUE)
plot(m1.nocov.log)
hist(residuals(m1.nocov.log))
qqnorm(residuals(m1.nocov.log))
dat.plot <- dat %>%
  select(id, sex_f, bmi, age, tug, time) %>%
  na.omit()


dat.plot %<>% mutate(yhat.3 = predict(m3.nocov.log, type = "response"))

# Plot mean response over time
ggplot(data = dat.plot, aes(x = time, y = tug)) +
  geom_point(aes(x = time, y = tug)) +
  geom_line(aes(group = id, x = time, y = tug), colour = "black", size = 1) +
  geom_smooth(aes(x = time, y = exp(yhat.3)), method = loess) +
  # coord_cartesian(xlim = c(0, 800), ylim = c(0,40))+
  theme_bw()


# Calibration plot
ggplot(data = dat.plot) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(x = log(tug), y = yhat.3))




# ----------- AIC Comparisons --------------------------------------------------
m1.nocov.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time.center + I(time.center^2) + 
                       (1 | id) + (0 + time.center | id) + (0 + I(time.center^2) | id),
                     data = dat,
                     REML = FALSE)


m2.int.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + k.40 + (1 | id),
                   data = dat,
                   REML = FALSE)

m3.nocov.log <- lmer(log(tug) ~ 1 + bmi + sex_f + age + time + (1 | id) + (1 | time),
                     data = dat,
                     REML = FALSE)


AIC(m1.nocov.log) 
# [1] 695.5576
AIC(m2.int.log) 
# [1] 227.3472
AIC(m3.nocov.log) 
# [1] 477.1535
