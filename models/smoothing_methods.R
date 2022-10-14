##########################################
# Modeling approaches for NCC
# Oct 2022
# Mbr
##########################################

rm(list = ls())
library(NCC)
library(ggplot2)
library(tidyverse)

##########################################
# Simulated trial with linear time trends
db_l <- datasim_cont(num_arms = 3, n_arm = 100, d = c(0, 100, 250),
                     theta = rep(0, 3), lambda = rep(1, 4), sigma = 1, trend = "linear", full=T)$Data

# fixmodel_cont(data = db_l, arm = 3)
# mixmodel_cont(data = db_l, arm = 3)

mod <- lm(response ~ as.factor(treatment) + as.factor(period), db_l)
summary(mod)

n=dim(db_l)[1]
db_l$pred = predict(mod)
db_l$treatment = as.factor(db_l$treatment)
pl=ggplot(db_l) +
  geom_point(aes(x = 1:n/n, y = response, color = treatment)) +
  geom_line(aes(x = 1:n/n, y = pred, color = treatment),size=1.3) +
  xlab("Proportion total sample size") + ylab("Responses")
pl
# 
# 
# lowess(db_l$Data$j, db_l$Data$response, f=0.5, iter=0)
# plot(db_l$Data$j, db_l$Data$response)
# lines(lowess(db_l$Data$j, db_l$Data$response, f=0.5, iter=0), lwd=2)

##########################################
# Simulated trial with inverse-u time trends
db_inv <- datasim_cont(num_arms = 3, n_arm = 100, d = c(0, 100, 250),
                     theta = rep(0, 3), lambda = rep(5, 4), sigma = 1, trend = "inv_u", N_peak = 200, full = T)$Data
plot_trial(treatments = db_inv$treatment)

mod <- lm(response ~ as.factor(treatment) + as.factor(period), db_inv)
summary(mod)

n=dim(db_inv)[1]
db_inv$pred = predict(mod)
db_inv$treatment = as.factor(db_inv$treatment) 
pl=ggplot(db_inv) +
  geom_point(aes(x = 1:n/n, y = response, color = treatment)) +
  geom_line(aes(x = 1:n/n, y = pred, color = treatment),size=1.3) + 
  xlab("Proportion total sample size") + ylab("Responses")
pl

##########################################
# Approach 1: lowess / Loess
# non-parametric approach that fits multiple regressions in local neighborhood
# least squares regression is used in each window

db_inv_f = db_inv
db_inv <- db_inv_f %>%  filter(treatment == 0)

# lowess function 
lowess(db_inv$j, db_inv$response, f=0.5, iter=0)
plot(db_inv$j, db_inv$response)
lines(lowess(db_inv$j, db_inv$response, f=0.5, iter=0), lwd=2)

# loess function 

# first adjust for patients' entry times
loessMod10 <- loess(response ~ j, data=db_inv, span=0.10) # 10% smoothing span
loessMod25 <- loess(response ~ j, data=db_inv, span=0.25) # 25% smoothing span
loessMod50 <- loess(response ~ j, data=db_inv, span=0.50) # 50% smoothing span
loessMod75 <- loess(response ~ j, data=db_inv, span=0.75) # 50% smoothing span
loessMod1 <- loess(response ~ j, data=db_inv, span=1)

# first adjust for time buckets of size 5
unit_size=5
# timebuckets <- 1:(length(db_inv$response)/5)
db_inv$cal_time <- rep(c(1:ceiling((nrow(db_inv)/unit_size))), each=unit_size)[1:nrow(db_inv)]
loessMod10_cal <- loess(response ~ cal_time, data=db_inv, span=0.10) # 10% smoothing span
loessMod25_cal <- loess(response ~ cal_time, data=db_inv, span=0.25) # 25% smoothing span
loessMod50_cal <- loess(response ~ cal_time, data=db_inv, span=0.50) # 50% smoothing span
loessMod75_cal <- loess(response ~ cal_time, data=db_inv, span=0.75) # 50% smoothing span


# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 
smoothed75 <- predict(loessMod75) 
smoothed1 <- predict(loessMod1) 


smoothed10_cal <- predict(loessMod10_cal) 
smoothed25_cal <- predict(loessMod25_cal) 
smoothed50_cal <- predict(loessMod50_cal) 
smoothed75_cal <- predict(loessMod75_cal) 

# Plot it
plot(db_inv$j, db_inv$response, pch=19, cex=0.2, xlab = "Time", ylab= "Response in control arm")
lines(smoothed10, x=db_inv$j, col="red", lwd=2)
lines(smoothed25, x=db_inv$j, col="green", lwd=2)
lines(smoothed50, x=db_inv$j, col="blue", lwd=2)
lines(smoothed75, x=db_inv$j, col="orange", lwd=2)
lines(smoothed1, x=db_inv$j, col="black", lwd=2)

lines(smoothed10_cal, x=db_inv$j, col="red", lwd=2, lty=2)
lines(smoothed25_cal, x=db_inv$j, col="green", lwd=2, lty=2)
lines(smoothed50_cal, x=db_inv$j, col="blue", lwd=2, lty=2)
lines(smoothed75_cal, x=db_inv$j, col="orange", lwd=2, lty=2)

# loess(response ~ cal_time, data=db_inv, span=0.50, model=T)

##########################################
# Approach 2: Kernel methods

# https://cran.r-project.org/web/packages/ks/ks.pdf

##########################################
# Approach 3: Splines

# require("splines")

##########################################

# Regression with fixed effects and smoothed time estimator
# Regression with random effects and smoothed time estimator
# - linear splines
# - quadratic splines
# - cubic splines
# - nonparametric

# Mixed models with splines
require("lme4")
require("splines")  
# lmer(counts ~ dependent_variable + (bs(t) | ID), family="poisson")
# gam(counts ~ dependent_variable + ID + s(t, by = ID) , family="poisson") 


mod_spl <- lm(response ~ as.factor(treatment) + bs(j, df = 3), data = db_inv_f)
summary(mod_spl)

n=dim(db_inv_f)[1]
db_inv_f$pred = predict(mod_spl)
db_inv_f$treatment = as.factor(db_inv_f$treatment) 
pl=ggplot(db_inv_f) +
  geom_point(aes(x = 1:n/n, y = response, color = treatment)) +
  geom_line(aes(x = 1:n/n, y = pred, color = treatment),size=1.3) + 
  xlab("Proportion total sample size") + ylab("Responses")
pl

# splines with random effects
mod_spl_r <- lmer(response ~ as.factor(treatment) + (bs(j, df = 3) | period ),  data = db_inv_f) 
summary(mod_spl_r)
