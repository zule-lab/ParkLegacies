# looking at model 1 posterior 
library(dplyr)
library(ggplot2)  
library(tidyr)# ggplot, dplyr, %>%, and friends
library(brms)         # Bayesian modeling through Stan
library(tidybayes)    # Manipulate Stan objects in a tidy way
library(emmeans)      # Calculate marginal effects in even fancier ways
library(patchwork)    # Combine ggplot objects

epred_dens <- model_1_brms_sample %>% 
  epred_draws(expand_grid(mean_day_con_s = 0,
                              Dens_L_s = seq(-3, 3, 0.5),
                              DBH_med_L_s = 0,
                              SR_L_s = 0),
              re_formula = NA)
ggplot(epred_dens, aes(x = .epred)) +
  stat_halfeye() +
  labs(x = "Tree Density", y = "Density") +
  theme_classic() +
  theme(legend.position = "bottom")

epred_DBH <- model_1_brms_sample %>% 
  epred_draws(expand_grid(mean_day_con_s = 0,
                          Dens_L_s = 0,
                          DBH_med_L_s = seq(-3, 3, 0.5),
                          SR_L_s = 0),
              re_formula = NA)
ggplot(epred_DBH, aes(x = .epred)) +
  stat_halfeye() +
  labs(x = "Tree Size", y = "Density") +
  theme_classic() +
  theme(legend.position = "bottom")

epred_con <- model_1_brms_sample %>% 
  epred_draws(expand_grid(mean_day_con_s = seq(-3, 3, length.out = 500),
                          Dens_L_s = 0,
                          DBH_med_L_s = 0,
                          SR_L_s = 0),
              re_formula = NA)
ggplot(epred_con, aes(x = .epred)) +
  stat_halfeye() +
  labs(x = "Control Temp", y = "Density") +
  theme_classic() +
  theme(legend.position = "bottom")

epred_SR <- model_1_brms_sample %>% 
  epred_draws(expand_grid(mean_day_con_s = 0,
                          Dens_L_s = 0,
                          DBH_med_L_s = 0,
                          SR_L_s = seq(-3, 3, 0.5)),
              re_formula = NA)
ggplot(epred_SR, aes(x = .epred)) +
  stat_halfeye() +
  labs(x = "Species Richness", y = "Density") +
  theme_classic() +
  theme(legend.position = "bottom")


pred <- predictions(model_1_brms_sample,
                    newdata = datagrid(Park = real_data_cooling$Park,
                                       mean_day_con_s = 0, 
                                       Dens_L_s = 0,
                                       DBH_med_L_s = seq(-3, 3, length.out = 100),
                                       SR_L_s = 0)) |> 
  posterior_draws()

t <- ggplot(pred, aes(x = DBH_med_L_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(~ Park)
