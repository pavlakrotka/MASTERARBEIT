#devtools::install_github("pavlakrotka/NCC", build = TRUE, force=T)
library(NCC)
library(tidyverse)

n_sim <- 10000
set.seed(123)


# SCENARIO I - 3 arms, equal time trends

#get_ss_matrix(num_arms = 3, n_arm = 3000, d = c(0,3000,7500))

scenario_i_eq_alpha <- data.frame(num_arms = 3, 
                                  n_arm = 3000, 
                                  d1 = 0,
                                  d2 = 3000,
                                  d3 = 7500,
                                  period_blocks = 2, 
                                  mu0 = 0,
                                  sigma = 1,
                                  theta1 = 0.075,
                                  theta2 = 0,
                                  theta3 = 0,
                                  lambda0 = seq(-0.15, 0.15, length.out = 9), 
                                  lambda1 = seq(-0.15, 0.15, length.out = 9),
                                  lambda2 = seq(-0.15, 0.15, length.out = 9),
                                  lambda3 = seq(-0.15, 0.15, length.out = 9),
                                  trend = c(rep("linear", 9), rep("stepwise", 9)),
                                  alpha = 0.025) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_i_eq_alpha <- sim_study(nsim = n_sim, scenarios = scenario_i_eq_alpha, models = c("fixmodel", "fixmodel_cal", "sepmodel", "mixmodel", "mixmodel_cal"), endpoint = "cont")
write_csv(results_i_eq_alpha, "results/results_i_eq_alpha.csv")


#power.t.test(n=3000, sd=1, sig.level = 0.025, power=0.8, type = "two.sample", alternative = "one.sided")

scenario_i_eq_pow <- data.frame(num_arms = 3, 
                                n_arm = 3000, 
                                d1 = 0,
                                d2 = 3000,
                                d3 = 7500,
                                period_blocks = 2, 
                                mu0 = 0,
                                sigma = 1,
                                theta1 = 0.075,
                                theta2 = 0.075,
                                theta3 = 0.075,
                                lambda0 = seq(-0.15, 0.15, length.out = 9), 
                                lambda1 = seq(-0.15, 0.15, length.out = 9),
                                lambda2 = seq(-0.15, 0.15, length.out = 9),
                                lambda3 = seq(-0.15, 0.15, length.out = 9),
                                trend = c(rep("linear", 9), rep("stepwise", 9)),
                                alpha = 0.025) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_i_eq_pow <- sim_study(nsim = n_sim, scenarios = scenario_i_eq_pow, models = c("fixmodel", "fixmodel_cal", "sepmodel", "mixmodel", "mixmodel_cal"), endpoint = "cont")
write_csv(results_i_eq_pow, "results/results_i_eq_pow.csv")







# 
# trial_data <- datasim_cont(num_arms = 3,
#                            n_arm = 3000,
#                            d = c(0,3000,7500),
#                            period_blocks = 2,
#                            mu0 = 0,
#                            sigma = 1,
#                            theta = c(0, 0, 0),
#                            lambda = rep(0.5, 4),
#                            trend = "stepwise")
# 
# mixmodel_cal_cont(trial_data, arm=2)
# 
# library(spaMM)
# mod <- fitme(response ~ 1 + as.factor(treatment) + AR1(1|period), data = trial_data, method="REML")
# 
# mod
# 
# summary(mod)
# 
# 
# ggplot(trial_data) +
#   geom_point(aes(x=1:nrow(trial_data), y=response, color=period))
# 
# 
# cor(trial_data[trial_data$period==2 & trial_data$treatment==0,"response"], trial_data[trial_data$period==3 & trial_data$treatment==0,"response"])






























