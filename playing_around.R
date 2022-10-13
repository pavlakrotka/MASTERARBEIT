library(tidyverse)
library(NCC)
library(lmerTest)
library(spaMM)
library(mgcv)
library(brms)
library(forecast)

trial_data <- datasim_cont(num_arms = 10, n_arm = 250, d = 250*seq(0,9), theta = rep(0.25, 10), lambda = rep(0.15, 11), sigma = 1, trend = "stepwise")

mod <- lmer(response ~ as.factor(treatment) + (1 | period), trial_data) # using lmerTest
res <- summary(mod)
mod
res


mod_ <- lm(response ~ as.factor(treatment) + as.factor(period), trial_data)
res_ <- summary(mod_)
mod_
res_


mod_ar <- fitme(response ~ as.factor(treatment) + AR1(1 | period), trial_data, method="REML")
res_ar <- summary.HLfit(mod_ar, verbose = FALSE)
mod_ar
res_ar

Corr(mod_ar) # correlation random effects
vcov.HLfit(mod_ar) # variance-covariance fixed effects
VarCorr(mod_ar) # variance random effect + residual variance


# No trend


trial_data <- datasim_cont(num_arms = 10, n_arm = 250, d = 250*seq(0,9), theta = rep(0.25, 10), lambda = rep(0, 11), sigma = 1, trend = "stepwise")

mod_ar <- fitme(response ~ as.factor(treatment) + AR1(1 | period), trial_data, method="REML")
res_ar <- summary.HLfit(mod_ar, verbose = FALSE)
mod_ar
res_ar

Corr(mod_ar) # correlation random effects
vcov.HLfit(mod_ar) # variance-covariance fixed effects
VarCorr(mod_ar) # variance random effect + residual variance


# Separate vs. separate adjusted

arm <- 10
periods <- unique(trial_data[trial_data$treatment==arm,]$period)
data_new <- trial_data[trial_data$treatment %in% c(0, arm) & trial_data$period %in% periods,]

summary(lm(response ~ as.factor(treatment) + as.factor(period), data_new))
summary(lm(response ~ as.factor(treatment), data_new))


# 4 arms

trial_data <- datasim_cont(num_arms = 4, n_arm = 250, d = 250*seq(0,3), theta = rep(0.25, 4), lambda = rep(5, 5), sigma = 1, trend = "stepwise", N_peak = 700, full = T)$Data

## Fixmodel

mod_lm <- lm(response ~ as.factor(treatment) + as.factor(period), trial_data)
res_lm <- summary(mod_lm)
mod_lm
res_lm

trial_data$fitted_lm <- fitted(mod_lm)

## AR1

mod_ar <- fitme(response ~ as.factor(treatment) + AR1(1 | period), trial_data, method="REML")
res_ar <- summary.HLfit(mod_ar, verbose = FALSE)
mod_ar
res_ar

Corr(mod_ar) # correlation random effects
vcov.HLfit(mod_ar) # variance-covariance fixed effects
VarCorr(mod_ar) # variance random effect + residual variance


## Mixed - independent random effects

mod <- lmer(response ~ as.factor(treatment) + (1 | period), trial_data) # using lmerTest
res <- summary(mod)
mod
res

ranef(mod)


gam <- gam(
  response ~ as.factor(treatment) + s(period, bs = "cr", k = 5), 
  data = trial_data, 
  method = "REML"
)

summary(gam)

trial_data$fitted_gam <- fitted(gam)

ggplot(trial_data) + 
  aes(x = j, y = response) + 
  geom_point(alpha = .5) + 
  geom_line(aes(y = fitted_gam), color = "blue") #+
  #geom_line(aes(y = fitted_lm), color = "red")



# Construct a smoothing basis outside of a model
sm_raw <- smoothCon(
  s(period, k = 3, bs = "cr"), 
  data = trial_data, 
  absorb.cons = TRUE, 
  diagonal.penalty = TRUE
)

sm_raw

re <- smooth2random(sm_raw[[1]], "", type = 2)

re
re$Xf

# 1 fixed effect and 18 random effect columns
mixed_matrix <- cbind(re$Xf, re$rand$Xr)
ggmatplot::ggmatplot(mixed_matrix) + 
  labs(title = NULL)

mod_1 <- brm(
  response ~ as.factor(treatment) + (1 | period), 
  trial_data, 
  family = gaussian, 
  #file = "radon"
)

mod_1



b_gam <- brm(
  response ~ as.factor(treatment) + s(period, bs = "cr", k = 5),
  data = trial_data, 
  family = gaussian,
  #file = "b-gam"
)

summary(b_gam)


# Time series

fit2 <- auto.arima(trial_data$response, xreg=as.matrix(trial_data[,3:4]))
fit2





























