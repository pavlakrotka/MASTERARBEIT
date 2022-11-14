#devtools::install_github("pavlakrotka/NCC@v1.0", build = TRUE, force=T)
library(NCC)
library(tidyverse)

n_sim <- 10000
set.seed(123)


# SCENARIO II - 10 arms, equal time trends, vary overlaps

get_ss_matrix(num_arms = 10, n_arm = 250, d = 250*(0:9))

scenario_ii_eq_alpha <- data.frame(num_arms = 10, 
                                   n_arm = 250, 
                                   d1 = seq(0, 500, by=100)*0,
                                   d2 = seq(0, 500, by=100)*1,
                                   d3 = seq(0, 500, by=100)*2,
                                   d4 = seq(0, 500, by=100)*3,
                                   d5 = seq(0, 500, by=100)*4,
                                   d6 = seq(0, 500, by=100)*5,
                                   d7 = seq(0, 500, by=100)*6,
                                   d8 = seq(0, 500, by=100)*7,
                                   d9 = seq(0, 500, by=100)*8,
                                   d10 = seq(0, 500, by=100)*9,
                                   period_blocks = 2,
                                   unit_size = 25,
                                   mu0 = 0,
                                   sigma = 1,
                                   theta1 = 0.25,
                                   theta2 = 0,
                                   theta3 = 0,
                                   theta4 = 0,
                                   theta5 = 0,
                                   theta6 = 0,
                                   theta7 = 0,
                                   theta8 = 0,
                                   theta9 = 0,
                                   theta10 = 0,
                                   lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each=5), 
                                   lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda5 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda6 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda7 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda8 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda9 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   lambda10 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                   trend = c(rep("linear", 45), rep("stepwise_2", 45)),
                                   alpha = 0.025,
                                   ci = FALSE) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_ii_eq_alpha <- sim_study_par(nsim = n_sim, scenarios = scenario_ii_eq_alpha, models = c("fixmodel", "fixmodel_cal", "sepmodel", "poolmodel", "mixmodel", "mixmodel_cal", "mixmodel_AR1", "mixmodel_AR1_cal"), endpoint = "cont")
write_csv(results_ii_eq_alpha, "results/results_ii_eq_alpha.csv")


#power.t.test(n=250, sd=1, sig.level = 0.025, power=0.8, type = "two.sample", alternative = "one.sided")

scenario_ii_eq_pow <- data.frame(num_arms = 10, 
                                 n_arm = 250, 
                                 d1 = seq(0, 500, by=100)*0,
                                 d2 = seq(0, 500, by=100)*1,
                                 d3 = seq(0, 500, by=100)*2,
                                 d4 = seq(0, 500, by=100)*3,
                                 d5 = seq(0, 500, by=100)*4,
                                 d6 = seq(0, 500, by=100)*5,
                                 d7 = seq(0, 500, by=100)*6,
                                 d8 = seq(0, 500, by=100)*7,
                                 d9 = seq(0, 500, by=100)*8,
                                 d10 = seq(0, 500, by=100)*9,
                                 period_blocks = 2,
                                 unit_size = 25,
                                 mu0 = 0,
                                 sigma = 1,
                                 theta1 = 0.25,
                                 theta2 = 0.25,
                                 theta3 = 0.25,
                                 theta4 = 0.25,
                                 theta5 = 0.25,
                                 theta6 = 0.25,
                                 theta7 = 0.25,
                                 theta8 = 0.25,
                                 theta9 = 0.25,
                                 theta10 = 0.25,
                                 lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each=5), 
                                 lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda5 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda6 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda7 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda8 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda9 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 lambda10 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                 trend = c(rep("linear", 45), rep("stepwise_2", 45)),
                                 alpha = 0.025,
                                 ci = FALSE) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_ii_eq_pow <- sim_study_par(nsim = n_sim, scenarios = scenario_ii_eq_pow, models = c("fixmodel", "fixmodel_cal", "sepmodel", "poolmodel", "mixmodel", "mixmodel_cal", "mixmodel_AR1", "mixmodel_AR1_cal"), endpoint = "cont")
write_csv(results_ii_eq_pow, "results/results_ii_eq_pow.csv")








# trial_data <- datasim_cont(num_arms = 10,
#                            n_arm = 250,
#                            d = 500*(0:9),
#                            period_blocks = 2,
#                            mu0 = 0,
#                            sigma = 1,
#                            theta = rep(0, 10),
#                            lambda = rep(0.5, 11),
#                            trend = "linear")
# 
# #mixmodel_cont(trial_data, arm=10)
# 
# for (i in 2:10) {
#   print(mixmodel_cal_cont(trial_data, arm=i))
# }

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









# SCENARIO III - 4 arms, equal time trends, vary bucket size

get_ss_matrix(num_arms = 4, n_arm = 250, d = 250*(0:3))

scenario_iii_eq_alpha <- data.frame(num_arms = 4, 
                                    n_arm = 250, 
                                    d1 = 250*0,
                                    d2 = 250*1,
                                    d3 = 250*2,
                                    d4 = 250*3,
                                    unit_size = round(seq(15, 800, length.out=20)),
                                    period_blocks = 2, 
                                    mu0 = 0,
                                    sigma = 1,
                                    theta1 = 0.25,
                                    theta2 = 0,
                                    theta3 = 0,
                                    theta4 = 0,
                                    lambda0 = 0.15,
                                    lambda1 = 0.15,
                                    lambda2 = 0.15,
                                    lambda3 = 0.15,
                                    lambda4 = 0.15,
                                    trend = c(rep("linear", 20), rep("stepwise_2", 20)),
                                    alpha = 0.025,
                                    ci = FALSE) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_iii_eq_alpha <- sim_study_par(nsim = n_sim, scenarios = scenario_iii_eq_alpha, models = c("fixmodel", "fixmodel_cal", "sepmodel", "poolmodel", "mixmodel", "mixmodel_cal", "mixmodel_AR1", "mixmodel_AR1_cal"), endpoint = "cont")
write_csv(results_iii_eq_alpha, "results/results_iii_eq_alpha.csv")











scenario_iii_eq_pow <- data.frame(num_arms = 4, 
                                  n_arm = 250, 
                                  d1 = 250*0,
                                  d2 = 250*1,
                                  d3 = 250*2,
                                  d4 = 250*3,
                                  unit_size = round(seq(15, 800, length.out=20)),
                                  period_blocks = 2, 
                                  mu0 = 0,
                                  sigma = 1,
                                  theta1 = 0.25,
                                  theta2 = 0.25,
                                  theta3 = 0.25,
                                  theta4 = 0.25,
                                  lambda0 = 0.15, 
                                  lambda1 = 0.15,
                                  lambda2 = 0.15,
                                  lambda3 = 0.15,
                                  lambda4 = 0.15,
                                  trend = c(rep("linear", 20), rep("stepwise_2", 20)),
                                  alpha = 0.025,
                                  ci = FALSE) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_iii_eq_pow <- sim_study_par(nsim = n_sim, scenarios = scenario_iii_eq_pow, models = c("fixmodel", "fixmodel_cal", "sepmodel", "poolmodel", "mixmodel", "mixmodel_cal", "mixmodel_AR1", "mixmodel_AR1_cal"), endpoint = "cont")
write_csv(results_iii_eq_pow, "results/results_iii_eq_pow.csv")

















