devtools::install_github("pavlakrotka/NCC", build = TRUE, force=T)
library(NCC)
library(tidyverse)

n_sim <- 1000
set.seed(123)

# SCENARIO III - 4 arms, equal time trends, vary bucket size

get_ss_matrix(num_arms = 4, n_arm = 250, d = 250*(0:3))

scenario_iii_gam_eq_alpha <- data.frame(num_arms = 4, 
                                        n_arm = 250, 
                                        d1 = 250*0,
                                        d2 = 250*1,
                                        d3 = 250*2,
                                        d4 = 250*3,
                                        period_blocks = 2, 
                                        mu0 = 0,
                                        sigma = 1,
                                        theta1 = 0.25,
                                        theta2 = 0,
                                        theta3 = 0,
                                        theta4 = 0,
                                        lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                        lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                        lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                        lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                        lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                        trend = c(rep("linear", 45), rep("stepwise_2", 45)),
                                        alpha = 0.025,
                                        ci = FALSE,
                                        smoothing_basis = c("tp", "cr", "ps", "re", "gp"),
                                        basis_dim = -1,
                                        gam_method = "GCV.Cp") %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_iii_gam_eq_alpha <- sim_study_par(nsim = n_sim, scenarios = scenario_iii_gam_eq_alpha, models = c("gam"), endpoint = "cont")
write_csv(results_iii_gam_eq_alpha, "results/results_iii_gam_eq_alpha.csv")



scenario_iii_gam_eq_pow <- data.frame(num_arms = 4, 
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
                                      lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each=5), 
                                      lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                      lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                      lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                      lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each=5),
                                      trend = c(rep("linear", 45), rep("stepwise_2", 45)),
                                      alpha = 0.025,
                                      ci = FALSE,
                                      smoothing_basis = c("tp", "cr", "ps", "re", "gp"),
                                      basis_dim = -1,
                                      gam_method = "GCV.Cp") %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2 & lambda1==lambda3, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0 & theta3==0, "H0", "H1"))

results_iii_gam_eq_pow <- sim_study_par(nsim = n_sim, scenarios = scenario_iii_gam_eq_pow, models = c("gam"), endpoint = "cont")
write_csv(results_iii_gam_eq_pow, "results/results_iii_gam_eq_pow.csv")

















