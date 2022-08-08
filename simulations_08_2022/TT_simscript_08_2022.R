#devtools::install_github("pavlakrotka/NCC", build = TRUE, force=T)
library(NCC)
library(tidyverse)

n_sim <- 100000
set.seed(123)


# SCENARIO I - 3 arms, equal time trends

#get_ss_matrix(num_arms = 3, n_arm = 300, d = c(0,300,750))

TT_scenario_i_eq_alpha <- data.frame(num_arms = 3, 
                                  n_arm = 300, 
                                  d1 = 0,
                                  d2 = 300,
                                  d3 = 750,
                                  period_blocks = 2, 
                                  mu0 = 0,
                                  sigma = 1,
                                  theta1 = 0.25,
                                  theta2 = 0.25,
                                  theta3 = 0.25,
                                  lambda0 = seq(-0.15, 0.15, length.out = 9), 
                                  lambda1 = seq(-0.15, 0.15, length.out = 9),
                                  lambda2 = seq(-0.15, 0.15, length.out = 9),
                                  lambda3 = seq(-0.15, 0.15, length.out = 9),
                                  trend = c(rep("linear", 9), rep("stepwise", 9)),
                                  alpha = 0.025) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta1==theta3, "H0", "H1"))

TT_results_i_eq_alpha <- TT_sim_study_par(nsim = n_sim, scenarios = TT_scenario_i_eq_alpha, arms=c(1,3), models = c("fixmodel", "indirect"), endpoint = "cont")
write_csv(TT_results_i_eq_alpha, "results/TT_results_i_eq_alpha.csv")


#power.t.test(n=300, sd=1, sig.level = 0.025, power=0.8, type = "two.sample", alternative = "one.sided")

TT_scenario_i_eq_pow <- data.frame(num_arms = 3, 
                                n_arm = 300, 
                                d1 = 0,
                                d2 = 300,
                                d3 = 750,
                                period_blocks = 2, 
                                mu0 = 0,
                                sigma = 1,
                                theta1 = 0.25,
                                theta2 = 0.25,
                                theta3 = 0.5,
                                lambda0 = seq(-0.15, 0.15, length.out = 9), 
                                lambda1 = seq(-0.15, 0.15, length.out = 9),
                                lambda2 = seq(-0.15, 0.15, length.out = 9),
                                lambda3 = seq(-0.15, 0.15, length.out = 9),
                                trend = c(rep("linear", 9), rep("stepwise", 9)),
                                alpha = 0.025) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta1==theta3, "H0", "H1"))

TT_results_i_eq_pow <- TT_sim_study_par(nsim = n_sim, scenarios = TT_scenario_i_eq_pow, arms=c(1,3), models = c("fixmodel", "indirect"), endpoint = "cont")
write_csv(TT_results_i_eq_pow, "results/TT_results_i_eq_pow.csv")







# 
# trial_data <- datasim_cont(num_arms = 3,
#                            n_arm = 3000,
#                            d = c(0,3000,7500),
#                            period_blocks = 2,
#                            mu0 = 0,
#                            sigma = 1,
#                            theta = c(0.25, 0, 0),
#                            lambda = c(0, 0, 0, 0),
#                            trend = "linear")
# 
# mixmodel_cal_cont(trial_data, arm=2)

