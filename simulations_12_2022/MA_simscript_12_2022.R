devtools::install_github("pavlakrotka/NCC", build = TRUE, force=T)
library(NCC)
library(tidyverse)

n_sim <- 10000


# SCENARIO III - 4 arms, equal time trends, vary time trend patterns and strengths, and degree of polynomials

get_ss_matrix(num_arms = 4, n_arm = 250, d = 250*(0:3))

set.seed(1)
scenario_iii_eq_alpha <- data.frame(num_arms = 4, 
                                    n_arm = 250, 
                                    d1 = 250*0,
                                    d2 = 250*1,
                                    d3 = 250*2,
                                    d4 = 250*3,
                                    period_blocks = 2, 
                                    mu0 = 0,
                                    sigma = 1,
                                    theta1 = 0,
                                    theta2 = 0,
                                    theta3 = 0,
                                    theta4 = 0,
                                    lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                    lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                    lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                    lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                    lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                    trend = c(rep("linear", 27), rep("stepwise_2", 27), rep("stepwise",27), rep("inv_u", 27), rep("seasonal", 27*2)),
                                    alpha = 0.025,
                                    ncc = TRUE,
                                    unit_size = 25,
                                    ci = FALSE,
                                    bs_degree = c(1,2,3),
                                    poly_degree = c(1,2,3),
                                    n_wave = c(rep(NA, 27*4), rep(1, 27), rep(2, 27)),
                                    N_peak = 750) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_iii_eq_alpha <- sim_study_par(nsim = n_sim, scenarios = scenario_iii_eq_alpha, models = c("splines", "splines_cal", "fixmodel", "fixmodel_cal", "mixmodel", "mixmodel_cal", "mixmodel_AR1", "mixmodel_AR1_cal", "piecewise", "piecewise_cal", "sepmodel", "poolmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_iii_eq_alpha, "results/results_iii_eq_alpha.csv")


set.seed(2)
scenario_iii_eq_pow <- data.frame(num_arms = 4, 
                                  n_arm = 250, 
                                  d1 = 250*0,
                                  d2 = 250*1,
                                  d3 = 250*2,
                                  d4 = 250*3,
                                  period_blocks = 2, 
                                  mu0 = 0,
                                  sigma = 1,
                                  theta1 = 0.25,
                                  theta2 = 0.25,
                                  theta3 = 0.25,
                                  theta4 = 0.25,
                                  lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each=3), 
                                  lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                  lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                  lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                  lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                  trend = c(rep("linear", 27), rep("stepwise_2", 27), rep("stepwise",27), rep("inv_u", 27), rep("seasonal", 27*2)),
                                  alpha = 0.025,
                                  ncc = TRUE,
                                  unit_size = 25,
                                  ci = FALSE,
                                  bs_degree = c(1,2,3),
                                  poly_degree = c(1,2,3),
                                  n_wave = c(rep(NA, 27*4), rep(1, 27), rep(2, 27)),
                                  N_peak = 750) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_iii_eq_pow <- sim_study_par(nsim = n_sim, scenarios = scenario_iii_eq_pow, models = c("splines", "splines_cal", "fixmodel", "fixmodel_cal", "mixmodel", "mixmodel_cal", "mixmodel_AR1", "mixmodel_AR1_cal", "piecewise", "piecewise_cal", "sepmodel", "poolmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_iii_eq_pow, "results/results_iii_eq_pow.csv")





# SCENARIO IV - 7 treatment arms, some of them entering sequentially, equal time trends, vary time trend patterns and strengths, and degree of polynomials


get_ss_matrix(num_arms = 7, n_arm = 250, d = 250*c(0,1,1,2,2,3,3))


set.seed(3)
scenario_iv_eq_alpha <- data.frame(num_arms = 7, 
                                   n_arm = 250, 
                                   d1 = 250*0,
                                   d2 = 250*1,
                                   d3 = 250*1,
                                   d4 = 250*2,
                                   d5 = 250*2,
                                   d6 = 250*3,
                                   d7 = 250*3,
                                   period_blocks = 2, 
                                   mu0 = 0,
                                   sigma = 1,
                                   theta1 = 0,
                                   theta2 = 0,
                                   theta3 = 0,
                                   theta4 = 0,
                                   theta5 = 0,
                                   theta6 = 0,
                                   theta7 = 0,
                                   lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                   lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                   lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                   lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                   lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                   lambda5 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                   lambda6 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                   lambda7 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                   trend = c(rep("linear", 27), rep("stepwise_2", 27), rep("stepwise",27), rep("inv_u", 27), rep("seasonal", 27*2)),
                                   alpha = 0.025,
                                   ncc = TRUE,
                                   unit_size = 25,
                                   ci = FALSE,
                                   bs_degree = c(1,2,3),
                                   poly_degree = c(1,2,3),
                                   n_wave = c(rep(NA, 27*4), rep(1, 27), rep(2, 27)),
                                   N_peak = 914) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_iv_eq_alpha <- sim_study_par(nsim = n_sim, scenarios = scenario_iv_eq_alpha, models = c("splines", "splines_cal", "fixmodel", "fixmodel_cal", "mixmodel", "mixmodel_cal", "mixmodel_AR1", "mixmodel_AR1_cal", "piecewise", "piecewise_cal", "sepmodel", "poolmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_iv_eq_alpha, "results/results_iv_eq_alpha.csv")





set.seed(4)
scenario_iv_eq_pow <- data.frame(num_arms = 7, 
                                 n_arm = 250, 
                                 d1 = 250*0,
                                 d2 = 250*1,
                                 d3 = 250*1,
                                 d4 = 250*2,
                                 d5 = 250*2,
                                 d6 = 250*3,
                                 d7 = 250*3,
                                 period_blocks = 2, 
                                 mu0 = 0,
                                 sigma = 1,
                                 theta1 = 0.25,
                                 theta2 = 0.25,
                                 theta3 = 0.25,
                                 theta4 = 0.25,
                                 theta5 = 0.25,
                                 theta6 = 0.25,
                                 theta7 = 0.25,
                                 lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                 lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                 lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                 lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                 lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                 lambda5 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                 lambda6 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                 lambda7 = rep(seq(-0.5, 0.5, length.out = 9), each=3),
                                 trend = c(rep("linear", 27), rep("stepwise_2", 27), rep("stepwise",27), rep("inv_u", 27), rep("seasonal", 27*2)),
                                 alpha = 0.025,
                                 ncc = TRUE,
                                 unit_size = 25,
                                 ci = FALSE,
                                 bs_degree = c(1,2,3),
                                 poly_degree = c(1,2,3),
                                 n_wave = c(rep(NA, 27*4), rep(1, 27), rep(2, 27)),
                                 N_peak = 914) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_iv_eq_pow <- sim_study_par(nsim = n_sim, scenarios = scenario_iv_eq_pow, models = c("splines", "splines_cal", "fixmodel", "fixmodel_cal", "mixmodel", "mixmodel_cal", "mixmodel_AR1", "mixmodel_AR1_cal", "piecewise", "piecewise_cal", "sepmodel", "poolmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_iv_eq_pow, "results/results_iv_eq_pow.csv")







# SCENARIO V - vary calendar time length

set.seed(5)
scenario_v_eq_alpha <- data.frame(num_arms = 4, 
                                  n_arm = 250, 
                                  d1 = 250*0,
                                  d2 = 250*1,
                                  d3 = 250*2,
                                  d4 = 250*3,
                                  period_blocks = 2, 
                                  mu0 = 0,
                                  sigma = 1,
                                  theta1 = 0,
                                  theta2 = 0,
                                  theta3 = 0,
                                  theta4 = 0,
                                  lambda0 = 0.15,
                                  lambda1 = 0.15,
                                  lambda2 = 0.15,
                                  lambda3 = 0.15,
                                  lambda4 = 0.15,
                                  trend = c(rep("linear", 60), rep("stepwise_2", 60), rep("stepwise", 60), rep("inv_u", 60), rep("seasonal", 60*2)),
                                  alpha = 0.025,
                                  ncc = TRUE,
                                  unit_size = rep(round(seq(15, 800, length.out=20)), each = 3),
                                  ci = FALSE,
                                  bs_degree = c(1,2,3),
                                  poly_degree = c(1,2,3),
                                  n_wave = c(rep(NA, 60*4), rep(1, 60), rep(2, 60)),
                                  N_peak = 750) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_v_eq_alpha <- sim_study_par(nsim = n_sim, scenarios = scenario_v_eq_alpha, models = c("splines_cal", "fixmodel", "fixmodel_cal", "mixmodel_cal", "mixmodel_AR1_cal", "piecewise_cal", "sepmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_v_eq_alpha, "results/results_v_eq_alpha.csv")



set.seed(6)
scenario_v_eq_pow <- data.frame(num_arms = 4, 
                                n_arm = 250, 
                                d1 = 250*0,
                                d2 = 250*1,
                                d3 = 250*2,
                                d4 = 250*3,
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
                                trend = c(rep("linear", 60), rep("stepwise_2", 60), rep("stepwise", 60), rep("inv_u", 60), rep("seasonal", 60*2)),
                                alpha = 0.025,
                                ncc = TRUE,
                                unit_size = rep(round(seq(15, 800, length.out=20)), each = 3),
                                ci = FALSE,
                                bs_degree = c(1,2,3),
                                poly_degree = c(1,2,3),
                                n_wave = c(rep(NA, 60*4), rep(1, 60), rep(2, 60)),
                                N_peak = 750) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_v_eq_pow <- sim_study_par(nsim = n_sim, scenarios = scenario_v_eq_pow, models = c("splines_cal", "fixmodel", "fixmodel_cal", "mixmodel_cal", "mixmodel_AR1_cal", "piecewise_cal", "sepmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_v_eq_pow, "results/results_v_eq_pow.csv")












































