library(NCC)
library(tidyverse)
library(latex2exp)
library(ggpubr)

##########################################################################################################

set.seed(5)
trial_data <- datasim_cont(num_arms = 3, n_arm = 100, d = c(0, 100, 250),
                           theta = rep(0.25, 3), lambda = rep(0.15, 4), sigma = 1, trend = "stepwise_2")
head(trial_data)

plot_trial(trial_data$treatment)
ggsave("plot_trial.png", width = 7, height = 4)


fixmodel_cont(trial_data, arm = 3, alpha = 0.025)
summary(fixmodel_cont(data = trial_data, arm = 3)$model)

fixmodel_cal_cont(trial_data, arm = 3, unit_size = 25, alpha = 0.025)

mixmodel_cont(trial_data, arm = 3, ci = T, alpha = 0.025)

mixmodel_cal_cont(trial_data, arm = 3, ci = T, alpha = 0.025)

mixmodel_AR1_cont(trial_data, arm = 3, ci = T, alpha = 0.025)

mixmodel_AR1_cal_cont(trial_data, arm = 3, ci = T, alpha = 0.025)

splines_cont(trial_data, arm = 3, bs_degree = 3, alpha = 0.025)

splines_cal_cont(trial_data, arm = 3, bs_degree = 3, alpha = 0.025)

##########################################################################################################

lambda_values <- rep(seq(-0.15, 0.15, length.out = 9), 2)
sim_scenarios <- data.frame(num_arms = 4, 
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
                            lambda0 = lambda_values,
                            lambda1 = lambda_values,
                            lambda2 = lambda_values,
                            lambda3 = lambda_values,
                            lambda4 = lambda_values,
                            trend = c(rep("linear", 9), rep("stepwise_2", 9)),
                            alpha = 0.025,
                            ncc = TRUE)

set.seed(5)
sim_results <- sim_study_par(nsim = 1000, 
                             scenarios = sim_scenarios, 
                             arms = 4,
                             models = c("fixmodel", "mixmodel", "splines", "sepmodel", "poolmodel"), 
                             endpoint = "cont")


ggplot(sim_results, aes(x=lambda0, y=reject_h0, color=model)) +
  geom_point() +
  geom_line() +
  facet_grid(~ trend) +
  geom_hline(aes(yintercept = 0.025), linetype = "dotted") +
  labs(x="Strength of time trend", y="Type I error", color="Analysis approach") +
  theme_bw()
ggsave("t1e.png", width = 7, height = 5)

ggplot(sim_results, aes(x=lambda0, y=bias, color=model)) +
  geom_point() +
  geom_line() +
  facet_grid(~ trend) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(x="Strength of time trend", y="Bias", color="Analysis approach") +
  theme_bw()
ggsave("bias.png", width = 7, height = 5)

ggplot(sim_results, aes(x=lambda0, y=MSE, color=model)) +
  geom_point() +
  geom_line() +
  facet_grid(~ trend) +
  labs(x="Strength of time trend", y="MSE", color="Analysis approach") +
  theme_bw()
ggsave("mse.png", width = 7, height = 5)


##########################################################################################################

data_lin <- rbind(datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "linear", full = T)$Data,
                  datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.35, 5), sigma = 1, trend = "linear", full = T)$Data)

data_step <- rbind(datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "stepwise_2", full = T)$Data,
                   datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.35, 5), sigma = 1, trend = "stepwise_2", full = T)$Data)

data_step <- data_step %>%
  mutate(means_1 = ifelse(lambda1==0.35 & period==1 & (row_number()%%2 | row_number()%%3 | row_number()%%5), NA, means))
  

data_inv_u_pos <- rbind(datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "inv_u", N_peak = 500, full = T)$Data %>% mutate(N_peak=500),
                        datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "inv_u", N_peak = 800, full = T)$Data %>% mutate(N_peak=800))

data_inv_u_pos <- data_inv_u_pos %>%
  mutate(means_1 = ifelse(N_peak==800 & j<=500 & (row_number()%%2 | row_number()%%3 | row_number()%%5), NA, means))


data_seasonal_1 <- rbind(datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "seasonal", n_wave = 1, full = T)$Data %>% mutate(n_wave=1),
                         datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "seasonal", n_wave = 2, full = T)$Data %>% mutate(n_wave=2))



ggarrange(ggplot(data_lin) +
            geom_point(aes(x=j, y=means, color=as.factor(lambda1)), size=0.6) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), color = "lambda", 
                 title = "Linear trend with varying lambda"),
          
          ggplot(data_step) +
            geom_point(aes(x=j, y=means_1, color=as.factor(lambda1)), size=0.6) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), color = "lambda", 
                 title = "Stepwise trend with varying lambda") +
            scale_y_continuous(breaks = unique(data_step$means), labels = unique(data_step$means)),
          
          ggplot(data_inv_u_pos) +
            geom_point(aes(x=j, y=means_1, color=as.factor(N_peak)), size=0.6) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), color = "N_peak", 
                 title = "Inverted-U trend with lambda=0.15 and varying N_peak") +
            scale_y_continuous(breaks = seq(0, 0.075, length.out=4), labels = seq(0, 0.075, length.out=4)),
          
          
          ggplot(data_seasonal_1) +
            geom_point(aes(x=j, y=means, color=as.factor(n_wave)), size=0.6) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), color = "n_wave",
                 title = "Seasonal trend with lambda=0.15 and varying n_wave") +
            scale_y_continuous(breaks = seq(-0.15, 0.15, length.out=5), labels = seq(-0.15, 0.15, length.out=5)),

          ncol = 2, nrow = 2)


ggsave("trend_examples.png", height = 6, width = 10)

