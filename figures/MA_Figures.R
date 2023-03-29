#devtools::install_github("pavlakrotka/NCC", build = TRUE, force=T)
library(NCC)
library(tidyverse)
library(latex2exp)
library(ggpubr)

#########################################################################################################################################################################################

# Time trend patterns

data_lin <- datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "linear", full = T)$Data

data_step <- datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "stepwise_2", full = T)$Data

data_inv_u_pos <- datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "inv_u", N_peak = 764, full = T)$Data

data_inv_u_neg <- datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(-0.15, 5), sigma = 1, trend = "inv_u", N_peak = 764, full = T)$Data

data_seasonal_1 <- datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "seasonal", n_wave = 1, full = T)$Data

data_seasonal_2 <- datasim_cont(num_arms = 4, n_arm = 250, d = 250*c(0:3), theta=rep(0, 4), lambda = rep(0.15, 5), sigma = 1, trend = "seasonal", n_wave = 2, full = T)$Data

ggarrange(ggplot(data_lin) +
            geom_point(aes(x=j, y=means), color="#111d4f") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), title = "Linear trend"),
          
          ggplot(data_step) +
            geom_point(aes(x=j, y=means), color="#111d4f") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), title = "Stepwise trend") +
            scale_y_continuous(breaks = seq(0, 0.45, by=0.15), labels = seq(0, 0.45, by=0.15)),
          
          ggplot(data_inv_u_pos) +
            geom_point(aes(x=j, y=means), color="#111d4f") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), title = TeX("Inverted-U trend with positive $\\lambda$")) +
            scale_y_continuous(breaks = seq(0, 0.075, length.out=4), labels = seq(0, 0.075, length.out=4)),
          
          ggplot(data_inv_u_neg) +
            geom_point(aes(x=j, y=means), color="#111d4f") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), title = TeX("Inverted-U trend with negative $\\lambda$")) +
            scale_y_continuous(breaks = seq(-0.075, 0, length.out=4), labels = seq(-0.075, 0, length.out=4)),
          
          ggplot(data_seasonal_1) +
            geom_point(aes(x=j, y=means), color="#111d4f") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), title = TeX("Seasonal trend with $\\psi=1$")) +
            scale_y_continuous(breaks = seq(-0.15, 0.15, length.out=5), labels = seq(-0.15, 0.15, length.out=5)),
          
          ggplot(data_seasonal_2) +
            geom_point(aes(x=j, y=means), color="#111d4f") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(x="Patient recruitment", y=TeX("Mean response under $H_0$"), title = TeX("Seasonal trend with $\\psi=2$")) +
            scale_y_continuous(breaks = seq(-0.15, 0.15, length.out=5), labels = seq(-0.15, 0.15, length.out=5)),
          ncol = 3, nrow = 2)


ggsave("trend_patterns.png", height = 6, width = 10)

#########################################################################################################################################################################################


















