devtools::install_github("pavlakrotka/NCC", build = TRUE, force=T)

library(NCC)
library(tidyverse)
library(lmerTest)

trial_data <- datasim_cont(num_arms = 3, n_arm = 100, d = c(0, 100, 250),
                           theta = rep(0, 3), lambda = rep(0.15, 4), sigma = 0.01, trend = "linear", full = T)$Data

unit_size <- 25
trial_data$cal_time <- rep(c(1:ceiling((nrow(trial_data)/unit_size))), each=unit_size)[1:nrow(trial_data)]


mod <- gam(response ~ as.factor(treatment) + s(j, bs = "cr", k = 20), data = trial_data)
res <- summary(mod)


mod_lm <- lm(response ~ as.factor(treatment) + bs(j, knots = knots, degree = 2), trial_data)
res_lm <- summary(mod_lm)


ggplot(trial_data) + 
  aes(x = j, y = response, color = as.factor(treatment)) + 
  geom_point() + 
  geom_line(aes(y = mod_lm$fitted.values), size=1.3)

summary(lm(response ~ as.factor(treatment) + poly(j, degree = 3, raw = T), trial_data))


mod_lm_c <- lm(response ~ as.factor(treatment) + bs(cal_time, knots = c(100/25, 200/25, 300/25, 400/25), degree = 2), trial_data)
res_lm_c <- summary(mod_lm_c)


ggplot(trial_data) + 
  aes(x = j, y = response, color = as.factor(treatment)) + 
  geom_point() + 
  geom_line(aes(y = mod_lm_c$fitted.values), size=1.3)



mod_lm_c <- lm(response ~ as.factor(treatment) + as.factor(cal_time), trial_data)
res_lm_c <- summary(mod_lm_c)


ggplot(trial_data) + 
  aes(x = cal_time, y = response, color = as.factor(treatment)) + 
  geom_point() + 
  geom_line(aes(y = mod_lm_c$fitted.values), size=1.3)




mod_lmm <- lmer(response ~ as.factor(treatment) + (bs(j, knots = knots, degree=1) | period), trial_data)
res_lmm <- summary(mod_lmm)

ggplot(trial_data) + 
  aes(x = j, y = response, color = as.factor(treatment)) + 
  geom_point() + 
  geom_line(aes(y = fitted(mod_lm)), size=1.3)


max(trial_data[trial_data$period==1,"j"])

knots <- c()

for (i in unique(trial_data$period)) {
  knots <- c(knots, max(trial_data[trial_data$period==i,"j"]))
}

knots <- knots[-length(knots)]

b_fn <- as.data.frame(bs(trial_data$j, knots = knots, degree = 1))


rowSums(b_fn)
colSums(b_fn)


bs(trial_data$j, knots = knots, degree = 2)









library(segmented)


trial_data <- datasim_cont(num_arms = 3, n_arm = 100, d = c(0, 100, 250),
                           theta = c(0.25, 0.5, 1), lambda = rep(5, 4), sigma = 1, trend = "inv_u", N_peak = 350, full = T)$Data

trial_data$cal_time <- rep(c(1:ceiling((nrow(trial_data)/25))), each=25)[1:nrow(trial_data)]

op = options(contrasts=c("contr.treatment","contr.sum"))
fit <- lm(response ~ as.factor(treatment)+poly(j, 1, raw=F)*as.factor(period), trial_data)

#segmented.fit <- segmented(fit, seg.Z = ~j)

summary(fit)


ggplot(trial_data) + 
  aes(x = j, y = response, color = as.factor(treatment)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit)), size=1.3)




fit_bs <- splines_cont(trial_data, arm = 3, bs_degree = 3)


ggplot(trial_data) + 
  aes(x = j, y = response, color = as.factor(treatment)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit_bs$model)), size=1.3)



fit_fix <- fixmodel_cont(trial_data, arm = 3)


ggplot(trial_data) + 
  aes(x = j, y = response, color = as.factor(treatment)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit_fix$model)), size=1.3)






fit_cal <- lm(response ~ as.factor(treatment)+poly(j, 3, raw=F)*as.factor(cal_time), trial_data)


summary(fit_cal)


ggplot(trial_data) + 
  aes(x = j, y = response, color = as.factor(treatment)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit_cal)), size=1.3)








trial_data_bin <- datasim_bin(num_arms = 3, n_arm = 100, d = c(0, 100, 250), p0 = 0.7, OR = rep(1.8, 3), lambda = rep(0.15, 4), trend="stepwise")


glm(response ~ as.factor(treatment) + as.factor(period), trial_data_bin, family = binomial)



ggplot(data=NULL) +
  geom_line(aes(x=c(1:500), y=sin((c(1:500)*0.1))))



ggplot(data=NULL) +
  geom_line(aes(x=c(1:500), y=0.15 * sin( 500*c(1:500)/(2*pi) ) ))



N<-500
x<-c(1:500)
beta<-1
lambda<-0.15


ggplot(data=NULL) +
  geom_line(aes(x=x, y=lambda*sin(beta*(2*pi)*(x-1)/(N-1)) ))











trial_data <- datasim_cont(num_arms = 3, n_arm = 100, d = c(0, 100, 250),
                           theta = c(0.25, 0.5, 1), lambda = rep(0.5, 4), sigma = .1, trend = "seasonal", n_wave = 1, full = T)$Data

fit <- splines_cont(trial_data, arm=3, bs_degree = 3)

ggplot(trial_data) +
  geom_point(aes(x=j, y=response, color=as.factor(treatment))) +
  geom_line(aes(x=j, y=fitted(fit$model), color=as.factor(treatment)), size=1)















