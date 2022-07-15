#devtools::install_github("pavlakrotka/NCC", build = TRUE, force=T)
library(NCC)
library(tidyverse)

n_sim <- 100000
set.seed(123)


# SCENARIO I - 2 arms, equal time trends, H0

# get_ss_matrix(num_arms = 2, n_arm = 3000, d = c(0,2000))

scenario_i_alpha <- data.frame(num_arms = 2, 
                               n_arm = 3000, 
                               d1 = 0,
                               d2 = 2000,
                               period_blocks = 2, 
                               mu0 = 0,
                               sigma = 1,
                               theta1 = 0.25,
                               theta2 = 0,
                               lambda0 = seq(-0.15, 0.15, length.out = 9), 
                               lambda1 = seq(-0.15, 0.15, length.out = 9),
                               lambda2 = seq(-0.15, 0.15, length.out = 9),
                               trend = c(rep("linear", 9), rep("stepwise", 9)),
                               alpha = 0.1) %>%
  mutate(timetrend = ifelse(lambda0==lambda1 & lambda1==lambda2, "EQ", "DIFF"),
         hypothesis = ifelse(theta2==0, "H0", "H1"))

results_i_alpha <- sim_study_par(nsim = n_sim, scenarios = scenario_i_alpha, models = c("fixmodel", "sepmodel", "poolmodel", "mixmodel"), endpoint = "cont")
write_csv(results_i_alpha, "results/results_i_alpha.csv")




# trial_data <- datasim_cont(num_arms = 2, 
#                            n_arm = 3000, 
#                            d = c(0, 2000),
#                            period_blocks = 2, 
#                            mu0 = 0,
#                            sigma = 1,
#                            theta = c(0.25, 0),
#                            lambda = c(0.15, 0.15, 0.15),
#                            trend = "linear")
# 
# mixmodel_cont(trial_data, arm=2)
# 
# # Period as random effect
# 
# lmerTest::lmer(response ~ as.factor(treatment) + (1 | period), trial_data)
# 
# summary(lmerTest::lmer(response ~ as.factor(treatment) + (1 | period), trial_data))
# 
# ranef(lmerTest::lmer(response ~ as.factor(treatment) + (1 | period), trial_data))
# 
# dotplot.ranef.mer(ranef(lmerTest::lmer(response ~ as.factor(treatment) + (1 | period), trial_data)))
# 
# # Period as both, fixed and random effect
# 
# lmerTest::lmer(response ~ as.factor(treatment) + as.factor(period) + (1 | period), trial_data)
# 
# summary(lmerTest::lmer(response ~ as.factor(treatment) + as.factor(period) + (1 | period), trial_data))
# 
# ranef(lmerTest::lmer(response ~ as.factor(treatment) + as.factor(period) + (1 | period), trial_data))
# 
# dotplot.ranef.mer(ranef(lmerTest::lmer(response ~ as.factor(treatment) + as.factor(period) + (1 | period), trial_data)))
