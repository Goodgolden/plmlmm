
library(tidyverse)
library(magrittr)
library(lme4)
library(jtools)
library(lmerTest)
library(nlme)
library(JMbayes)
library(gridExtra)

## Read in and format data -----------------------------------------------------
dat <-
  readRDS("data/tug_dat.rds") %>%
  select(id, age, bmi, sex_f, time, tug, base_tug, pre_tug) %>%
  mutate(id = as.factor(id))


feas.dat <-
  read.csv("Data/therapist_final_060321.csv") %>%
  select(-X, -FirstVisit, -DC_Date) %>%
  distinct() %>%
  filter(!is.na(time)) %>% 
  mutate(sex_f = as.factor(case_when(sex == "M" ~ 0,
                                     (sex == "F" ~ 1))),
         tug = case_when(
           is.na(TUG_2) ~ TUG_1,
           is.na(TUG_1) ~ TUG_2,
           !is.na(TUG_1) & !is.na(TUG_2) ~ pmin(TUG_1, TUG_2)),
         time = as.numeric(time)) %>%
  ## k.40 = as.numeric(if_else(time > 40 , time - 40 , 0)))
  ## time.center = (time - mean(time))/sd(time))
  rename(bmi = BMI)

View(feas.dat)

## Create preop and baseline TUG values #
feas.preop <- 
  filter(feas.dat, time < 0) %>%
  rename(pre_tug = tug) %>%
  select(ATI_Number, pre_tug, time) %>%
  group_by(ATI_Number) %>%
  arrange(desc(time), .by_group = TRUE) %>%
  slice(1L) %>%
  select(-time)

feas.baseline <- 
  filter(feas.dat, time > 0) %>%
  group_by(ATI_Number) %>%
  arrange(time, .by_group = TRUE) %>%
  slice(1L) %>%
  select(ATI_Number, tug, time) %>%
  rename(base_tug = tug,
         base_t = time)


## merge in new variables #
feas.dat <- feas.dat %>% 
  merge(feas.preop, by = "ATI_Number", all = "TRUE") %>%
  merge(feas.baseline, by = "ATI_Number", all = "TRUE")
# feas.dat %<>% mutate(base_ind = as.numeric(if_else(base_tug == tug, 1, 0)))


## restrict feas.dat to variables of interest #
feas.dat <- feas.dat %>%
  select(ATI_Number, sex_f, bmi, age,
         time, tug, pre_tug, base_tug) %>%
  rename(id = ATI_Number) %>% 
  filter(time > 0)

## Examine data closely --------------------------------------------------------
feas.dat <- feas.dat %>% 
  filter(!is.na(id)) %>% 
  filter(!is.na(bmi))

# attributes(feas.dat)
# str(feas.dat)
# summary(feas.dat)


## Merge dat and feas.dat ------------------------------------------------------
## Create indicator variable for dat vs. feas.dat (training vs. testing)

feas.dat <- feas.dat %>% mutate(feas = as.factor(1))
dat <- dat %>% mutate(feas = as.factor(0))

## Create indicator for baseline measurement used to estimate random effects
full.dat <- rbind(feas.dat, dat) %>%
  mutate(base_ind = if_else(base_tug == tug, 1, 0)) %>%
  ## create time-based variables
  mutate(time.center = (time - mean(time)) / sd(time),
         k.40 = as.numeric(if_else(time > 40, time - 40, 0)))
# View(full.dat)
# str(full.dat)

## Recreate linear mixed models ------------------------------------------------
m1 <- lme(log(tug) ~ 1 + bmi + sex_f + age + time.center + I(time.center^2),
          data = full.dat[full.dat$feas == 0, ],
          random = list(id = pdDiag(~ 1 + time.center + I(time.center^2))),
          na.action = na.exclude)

## Model 2 predictions won't work without including random slope terms
m2 <- lme(log(tug) ~ 1 + bmi + sex_f + age + time + k.40,
          data = full.dat[full.dat$feas == 0, ],
          random = list(id = pdDiag(~ 1 + time + k.40)),
          na.action = na.exclude)


m3 <- lme(log(tug) ~ 1 + bmi + sex_f + age + time,
          random = ~ 1 + time | id,
          data = full.dat[full.dat$feas == 0, ],
          na.action = na.exclude)

# Mon Jun 21 09:52:52 2021 ------------------------------
par(mar=c(1,1,1,1))
pairs(full.dat[, c(2:6, 8:12)], lower.panel = NULL)

summary(m1)
broom.mixed::glance(m1)
broom.mixed::tidy(m1)
broom.mixed::tidy(m2)

## Predict tug for each observation using appropriate model---------------------
## Filter data to include only baseline observations from feas.dat
## the part requires the package
test.base <- full.dat %>%
  filter(feas == 1) %>%
  filter(base_ind == 1)


## Create vector of time.center values associated with times 1:n
time.vector <- c(1:200)
time.vector <- (time.vector - mean(full.dat$time)) / sd(full.dat$time)


## Select m1, m2, or m3 using lmeObject command for predictions
m.pred.5 <- 
  IndvPred_lme(lmeObject = m1,
               newdata = test.base,
               timeVar = "time.center",
               all_times = TRUE,
               return_data = TRUE,
               interval = "prediction",
               times = time.vector,
               level = 0.5,
               M = 500)
# View(m.pred.5)
# m1_aug <- broom.mixed::augment(m1)
# View(m1_aug)

m.pred.9 <- 
  IndvPred_lme(lmeObject = m1,
               newdata = test.base,
               timeVar = "time.center",
               all_times = TRUE,
               return_data = TRUE,
               interval = "prediction",
               times = time.vector,
               level = 0.8,
               M = 500)


## convert predictions out of log scale
m.pred.5 %<>% mutate(
  yhat = exp(pred),
  c75 = exp(upp),
  c25 = exp(low))

m.pred.9 %<>% mutate(
  yhat = exp(pred),
  c90 = exp(upp),
  c10 = exp(low))

# Create time values from time.center
m.pred.5 %<>% mutate(time = time.center * sd(full.dat$time) + mean(full.dat$time))


m.pred.9 %<>% mutate(time = time.center * sd(full.dat$time) + mean(full.dat$time))

## Examine model performance ---------------------------------------------------
## select predicted variables of interest
m.pred.5 <- m.pred.5 %>% select(id, time, yhat, c75, c25) # %>% filter(time<120)
m.pred.9 <- m.pred.9 %>% select(id, time, c90, c10) # %>% filter(time<120)

## merge m1.5 and m19
m.pred <- merge(m.pred.5, m.pred.9, by = c("id", "time"))


## calculate precision
m.pred %<>% mutate(prec50 = c75 - c25,
                   prec80 = c90 - c10)

mean(na.omit(m.pred$prec50))
mean(na.omit(m.pred$prec80))

## Emerge observed and predicted values
compare <- merge(feas.dat, m.pred, by = "id", all = TRUE)


## Restrict merged dataset to observed timepoints
compare <- compare %>%
  filter(time.x == time.y) %>%
  filter(!is.na(prec50)) %>%
  filter(!is.na(prec80))


## Examine the average bias of yhat estimates
compare %<>% mutate(bias = (yhat - tug) / sd(na.omit(tug)))
mean(na.omit(compare$bias))


## Examine coverage of IQR range
coverage <- compare %>%
  filter(tug <= c75) %>%
  filter(tug >= c25)
coverage <- as.numeric(NROW(coverage)) / as.numeric(NROW(compare))



## calibration plot
ggplot(data = compare) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(x = tug, y = yhat)) +
  geom_smooth(aes(x = tug, y = yhat)) +
  theme_bw()


# ----------- Plot trajectories for sample patients ---------------------------------------------------------------------------------------

# Random patient.a
random.a <- m.pred %>% sample_n(1)

patient.a <- m.pred %>% filter(id %in% random.a$id)

plot.a <-
  ggplot(data = patient.a) +
  geom_line(data = full.dat, 
            aes(x = time, y = tug, group = id), 
            color = "light gray") +
  geom_line(aes(x = time, y = yhat), 
            color = "red") +
  geom_smooth(aes(x = time, y = c75), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c25), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c90), 
              color = "blue",
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c10), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  theme_bw() +
  scale_y_continuous(limits = c(3, 25)) +
  scale_x_continuous(limits = c(0, 120)) +
  ggtitle("Patient_A")
plot.a


# Random patient.b
random.b <- m.pred %>% 
  sample_n(1)
patient.b <- m.pred %>% 
  filter(id %in% random.b$id)

plot.b <-
  ggplot(data = patient.b) +
  geom_line(data = full.dat, 
            aes(x = time, y = tug, group = id), 
            color = "light gray") +
  geom_line(aes(x = time, y = yhat), 
            color = "red") +
  geom_smooth(aes(x = time, y = c75), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c25), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c90), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c10), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  theme_bw() +
  scale_y_continuous(limits = c(3, 25)) +
  scale_x_continuous(limits = c(0, 120)) +
  ggtitle("Patient_B")
plot.b

# Random patient.c
random.c <- m.pred %>% sample_n(1)
patient.c <- m.pred %>% filter(id %in% random.c$id)

plot.c <-
  ggplot(data = patient.c) +
  geom_line(data = full.dat, 
            aes(x = time, y = tug, group = id), 
            color = "light gray") +
  geom_line(aes(x = time, y = yhat), 
            color = "red") +
  geom_smooth(aes(x = time, y = c75), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c25), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9,
              se = FALSE) +
  geom_smooth(aes(x = time, y = c90), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c10), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  theme_bw() +
  scale_y_continuous(limits = c(3, 25)) +
  scale_x_continuous(limits = c(0, 120)) +
  ggtitle("Patient_C")
plot.c


# View(full.dat)
# plot.d <-
#   full.dat %>%
#   mutate(id = factor(id)) %>%
#   ggplot() +
#   geom_line(aes(x = time, y = tug, group = id), 
#             color = "light gray") +
#   theme_bw() +
#   scale_y_continuous(limits = c(3, 25)) +
#   scale_x_continuous(limits = c(0, 120)) +
#   facet_wrap("id")
# plot.d

# Random patient.d
random.d <- m.pred %>% sample_n(1)
patient.d <- m.pred %>% filter(id %in% random.d$id)

plot.d <-
  ggplot(data = patient.d) +
  geom_line(data = full.dat, 
            aes(x = time, y = tug, group = id), 
            color = "light gray") +
  geom_line(aes(x = time, y = yhat), 
            color = "red") +
  geom_smooth(aes(x = time, y = c75), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c25), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c90), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c10), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  theme_bw() +
  scale_y_continuous(limits = c(3, 25)) +
  scale_x_continuous(limits = c(0, 120)) +
  ggtitle("Patient_D")
plot.d


# Random patient.e
random.e <- m.pred %>% sample_n(1)

patient.e <- m.pred %>% filter(id %in% random.e$id)

plot.e <-
  ggplot(data = patient.e) +
  geom_line(data = full.dat, 
            aes(x = time, y = tug, group = id), 
            color = "light gray") +
  geom_line(aes(x = time, y = yhat), 
            color = "red") +
  geom_smooth(aes(x = time, y = c75), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c25), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c90), 
              color = "blue", 
              method = "loess", 
              span = 0.9,
              se = FALSE) +
  geom_smooth(aes(x = time, y = c10), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  theme_bw() +
  scale_y_continuous(limits = c(3, 25)) +
  scale_x_continuous(limits = c(0, 120)) +
  ggtitle("Patient_E")



# Random patient.f
random.f <- m.pred %>% sample_n(1)
patient.f <- m.pred %>% filter(id %in% random.f$id)

plot.f <-
  ggplot(data = patient.f) +
  geom_line(data = full.dat, 
            aes(x = time, y = tug, group = id), 
            color = "light gray") +
  geom_line(aes(x = time, y = yhat), 
            color = "red") +
  geom_smooth(aes(x = time, y = c75), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c25), 
              color = "blue", 
              linetype = "dotted", 
              method = "loess",
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c90), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  geom_smooth(aes(x = time, y = c10), 
              color = "blue", 
              method = "loess", 
              span = 0.9, 
              se = FALSE) +
  theme_bw() +
  scale_y_continuous(limits = c(3, 25)) +
  scale_x_continuous(limits = c(0, 120)) +
  ggtitle("Patient_F")


# Display plots A-D on one page

grid.arrange(plot.a, plot.b, plot.c,
             plot.d, plot.e, plot.f, 
             nrow = 2)
