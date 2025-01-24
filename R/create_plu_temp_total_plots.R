create_plu_temp_total_plots <- function(plu_temp_brms_sample, temp_plots){
  
  model_5 <- plu_temp_brms_sample 
  
  # plot
  t <- model_5 %>%
    epred_draws(expand_grid(PastLandUse = c('Agricultural', 'Forested', 'Industrial'),
                            Age_s = 0,
                            tod = c('day', 'night')),
                re_formula = ~ (1 | tod)) %>%
    ggplot(aes(x = .epred, fill = tod)) +
    stat_halfeye(alpha = 0.7) +
    scale_fill_manual(values = c("#CFA35E", "#45A291"), labels = c('Day', 'Night')) + 
    theme_classic() + 
    theme(legend.position = "top",
          strip.text.y = element_text(angle = 0),
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    labs( y = "",
          color = "", 
          fill = "") + 
    facet_wrap(~ PastLandUse, ncol = 1)
  #facet_grid(rows = vars(factor(PastLandUse, levels=c('Industrial', 'Agricultural', 'Forested'))))
  
  # get axis breaks 
  atx <- c(as.numeric(na.omit(layer_scales(t)$x$break_positions())))
  
  # unscale x axis 
  f <- t + 
    scale_x_continuous(name = "Cooling Effect (\u00B0C)", 
                       breaks = atx,
                       labels = round(atx * sd(temp_plots$cooling) + mean(temp_plots$cooling), 1))
  
  ggsave('graphics/plu_temp_total_plots.png', f, width = 12, height = 10, units = 'in')
  
  return(f)
  
}


# CONTRAST

#mod_draws <- model_5 %>%
#  emmeans(~ PastLandUse + tod + PastLandUse:tod,
#          at = list(PPastLandUse = c('Agricultural', 'Forested', 'Industrial'),
#                    Age_s = 0,
#                    tod = c('day', 'night')),
#          epred = TRUE, re_formula = NA) %>%
#  contrast(method = "pairwise") %>% 
#  gather_emmeans_draws() %>% 
#  filter(contrast == "Agricultural day - Forested day" | 
#         contrast == "Agricultural day - Industrial day" |
#         contrast == "Agricultural night - Forested night" |
#         contrast == "Agricultural night - Industrial night" |
#         contrast == "Forested day - Industrial day" |
#         contrast == "Forested night - Industrial night") %>% 
#  mutate(tod = case_when(str_detect(contrast, 'day') == T ~ 'day',
#                         str_detect(contrast, 'night') == T ~ 'night'),
#         contrast = case_when(contrast == "Agricultural day - Forested day" ~ "Agricultural - Forested",
#                              contrast == "Agricultural day - Industrial day" ~ "Agricultural - Industrial",
#                              contrast == "Agricultural night - Forested night" ~ "Agricultural - Forested",
#                              contrast == "Agricultural night - Industrial night" ~ "Agricultural - Industrial",
#                              contrast == "Forested day - Industrial day" ~ "Forested - Industrial",
#                              contrast == "Forested night - Industrial night" ~ "Forested - Industrial"))
#
#t <- ggplot(mod_draws, aes(x = .value, fill = contrast)) +
#  stat_halfeye(alpha = 0.7) +
#  scale_fill_manual(values = c( "#1e3d14", "#669d62", "#c2d6a4")) + 
#  labs(y = "Density", fill = "") +
#  theme_classic() +
#  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold',
#                                  color = "black")) + 
#  facet_wrap(~tod)
#
## get axis breaks 
#atx <- c(as.numeric(na.omit(layer_scales(t)$x$break_positions())))
#
## unscale x axis 
#f <- t + 
#  scale_x_continuous(name = "Relative Cooling Effect (\u00B0C)", 
#                     breaks = atx,
#                     labels = round(atx * sd(temp_plots$cooling) + mean(temp_plots$cooling), 1))
#
