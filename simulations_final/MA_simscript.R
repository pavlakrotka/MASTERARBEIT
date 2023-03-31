###########################################################################################################################################################################################
install.packages("NCC")
library(NCC)

n_sim <- 10000

# Treatment effects to be used
# power.t.test(n=250, sd=1, sig.level = 0.025, power=0.8, type = "two.sample", alternative = "one.sided") -> 0.25
###########################################################################################################################################################################################

# SETTING 1

# get_ss_matrix(num_arms = 10, n_arm = 250, d = 475*(0:9))

set.seed(1)
setting_1_alpha <- data.frame(num_arms = 10, 
                              n_arm = 250, 
                              d1 = seq(0, 500, by = 25)*0,
                              d2 = seq(0, 500, by = 25)*1,
                              d3 = seq(0, 500, by = 25)*2,
                              d4 = seq(0, 500, by = 25)*3,
                              d5 = seq(0, 500, by = 25)*4,
                              d6 = seq(0, 500, by = 25)*5,
                              d7 = seq(0, 500, by = 25)*6,
                              d8 = seq(0, 500, by = 25)*7,
                              d9 = seq(0, 500, by = 25)*8,
                              d10 = seq(0, 500, by = 25)*9,
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
                              theta8 = 0,
                              theta9 = 0,
                              theta10 = 0,
                              lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each = 21), 
                              lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda5 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda6 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda7 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda8 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda9 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              lambda10 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                              trend = "linear",
                              alpha = 0.025)

results_setting_1_alpha <- sim_study_par(nsim = n_sim, scenarios = setting_1_alpha, arms = c(2:10), models = c("fixmodel", "sepmodel", "poolmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_setting_1_alpha, "results/results_setting_1_alpha.csv")






set.seed(2)
setting_1_pow <- data.frame(num_arms = 10, 
                            n_arm = 250, 
                            d1 = seq(0, 500, by = 25)*0,
                            d2 = seq(0, 500, by = 25)*1,
                            d3 = seq(0, 500, by = 25)*2,
                            d4 = seq(0, 500, by = 25)*3,
                            d5 = seq(0, 500, by = 25)*4,
                            d6 = seq(0, 500, by = 25)*5,
                            d7 = seq(0, 500, by = 25)*6,
                            d8 = seq(0, 500, by = 25)*7,
                            d9 = seq(0, 500, by = 25)*8,
                            d10 = seq(0, 500, by = 25)*9,
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
                            theta8 = 0.25,
                            theta9 = 0.25,
                            theta10 = 0.25,
                            lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each = 21), 
                            lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda5 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda6 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda7 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda8 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda9 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            lambda10 = rep(seq(-0.5, 0.5, length.out = 9), each = 21),
                            trend = "linear",
                            alpha = 0.025)

results_setting_1_pow <- sim_study_par(nsim = n_sim, scenarios = setting_1_pow, arms = c(2:10), models = c("fixmodel", "sepmodel", "poolmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_setting_1_pow, "results/results_setting_1_pow.csv")



###########################################################################################################################################################################################


# SETTING 2 

# get_ss_matrix(num_arms = 4, n_arm = 250, d = 250*(0:3))

set.seed(3)
setting_2_alpha <- data.frame(num_arms = 4, 
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
                              lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                              lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                              lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                              lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                              lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                              trend = c(rep("linear", 270), rep("stepwise_2", 270), rep("inv_u", 270), rep("seasonal", 270*2)),
                              alpha = 0.025,
                              ncc = TRUE,
                              unit_size = rep(seq(25, 750, by = 25), 9),
                              n_wave = c(rep(NA, 270*3), rep(1, 270), rep(2, 270)),
                              N_peak = c(rep(NA, 270*2), rep(750, 270), rep(NA, 270*2)))

results_setting_2_alpha <- sim_study_par(nsim = n_sim, scenarios = setting_2_alpha, arms = c(2:4), models = c("fixmodel", "fixmodel_cal", "sepmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_setting_2_alpha, "results/results_setting_2_alpha.csv")



set.seed(4)
setting_2_pow <- data.frame(num_arms = 4, 
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
                            lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                            lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                            lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                            lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                            lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each = 30),
                            trend = c(rep("linear", 270), rep("stepwise_2", 270), rep("inv_u", 270), rep("seasonal", 270*2)),
                            alpha = 0.025,
                            ncc = TRUE,
                            unit_size = rep(seq(25, 750, by = 25), 9),
                            n_wave = c(rep(NA, 270*3), rep(1, 270), rep(2, 270)),
                            N_peak = c(rep(NA, 270*2), rep(750, 270), rep(NA, 270*2)))

results_setting_2_pow <- sim_study_par(nsim = n_sim, scenarios = setting_2_pow, arms = c(2:4), models = c("fixmodel", "fixmodel_cal", "sepmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_setting_2_pow, "results/results_setting_2_pow.csv")


###########################################################################################################################################################################################


# SETTING 3


# get_ss_matrix(num_arms = 4, n_arm = 250, d = 250*(0:3))

set.seed(5)
setting_3_alpha <- data.frame(num_arms = 4, 
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
                              lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                              lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                              lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                              lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                              lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                              trend = c(rep("linear", 27), rep("stepwise_2", 27), rep("inv_u", 27), rep("seasonal", 27*2)),
                              alpha = 0.025,
                              ncc = TRUE,
                              unit_size = rep(c(25, 50, 100), 9),
                              n_wave = c(rep(NA, 27*3), rep(1, 27), rep(2, 27)),
                              N_peak = c(rep(NA, 27*2), rep(750, 27), rep(NA, 27*2)))

results_setting_3_alpha <- sim_study_par(nsim = n_sim, scenarios = setting_3_alpha, arms = c(2:4), models = c("fixmodel", "mixmodel", "mixmodel_AR1", "mixmodel_cal", "mixmodel_AR1_cal", "sepmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_setting_3_alpha, "results/results_setting_3_alpha.csv")




set.seed(6)
setting_3_pow <- data.frame(num_arms = 4, 
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
                            lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                            lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                            lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                            lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                            lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each = 3),
                            trend = c(rep("linear", 27), rep("stepwise_2", 27), rep("inv_u", 27), rep("seasonal", 27*2)),
                            alpha = 0.025,
                            ncc = TRUE,
                            unit_size = rep(c(25, 50, 100), 9),
                            n_wave = c(rep(NA, 27*3), rep(1, 27), rep(2, 27)),
                            N_peak = c(rep(NA, 27*2), rep(750, 27), rep(NA, 27*2)))

results_setting_3_pow <- sim_study_par(nsim = n_sim, scenarios = setting_3_pow, arms = c(2:4), models = c("fixmodel", "mixmodel", "mixmodel_AR1", "mixmodel_cal", "mixmodel_AR1_cal", "sepmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_setting_3_pow, "results/results_setting_3_pow.csv")


###########################################################################################################################################################################################


# SETTING 4

# get_ss_matrix(num_arms = 7, n_arm = 250, d = 250*c(0,1,1,2,2,3,3))


set.seed(7)
setting_4_alpha <- data.frame(num_arms = 7, 
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
                              lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                              lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                              lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                              lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                              lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                              lambda5 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                              lambda6 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                              lambda7 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                              trend = c(rep("linear", 81), rep("stepwise_2", 81), rep("inv_u", 81), rep("seasonal", 81*2)),
                              alpha = 0.025,
                              ncc = TRUE,
                              unit_size = rep(c(25, 50, 100), each = 3),
                              ci = FALSE,
                              bs_degree = c(1, 2, 3),
                              n_wave = c(rep(NA, 81*3), rep(1, 81), rep(2, 81)),
                              N_peak = c(rep(NA, 81*2), rep(1115, 81), rep(NA, 81*2)))

results_setting_4_alpha <- sim_study_par(nsim = n_sim, scenarios = setting_4_alpha, arms = c(2:7), models = c("splines", "splines_cal", "fixmodel", "sepmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_setting_4_alpha, "results/results_setting_4_alpha.csv")




set.seed(8)
setting_4_pow <- data.frame(num_arms = 7, 
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
                            lambda0 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                            lambda1 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                            lambda2 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                            lambda3 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                            lambda4 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                            lambda5 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                            lambda6 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                            lambda7 = rep(seq(-0.5, 0.5, length.out = 9), each = 9),
                            trend = c(rep("linear", 81), rep("stepwise_2", 81), rep("inv_u", 81), rep("seasonal", 81*2)),
                            alpha = 0.025,
                            ncc = TRUE,
                            unit_size = rep(c(25, 50, 100), each = 3),
                            ci = FALSE,
                            bs_degree = c(1, 2, 3),
                            n_wave = c(rep(NA, 81*3), rep(1, 81), rep(2, 81)),
                            N_peak = c(rep(NA, 81*2), rep(1115, 81), rep(NA, 81*2)))

results_setting_4_pow <- sim_study_par(nsim = n_sim, scenarios = setting_4_pow, arms = c(2:7), models = c("splines", "splines_cal", "fixmodel", "sepmodel"), endpoint = "cont", perc_cores = 0.99)
write_csv(results_setting_4_pow, "results/results_setting_4_pow.csv")






