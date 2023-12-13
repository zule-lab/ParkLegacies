create_plu_temp_total <- function(model_list, temp_indices){
  
  # extract relevant model from list
  model_5 <- model_list[[7]]
  
  # plot
  t <- model_5 %>%
    epred_draws(expand_grid(PastLandUse = c('Agricultural', 'Forested', 'Industrial'),
                            Age_s = 0,
                            tod = c('day', 'night')),
                re_formula = ~ (1 | tod)) %>%
    ggplot(aes(x = .epred, fill = PastLandUse, color = tod)) +
    stat_halfeye() +
    scale_color_manual(values = c("#CFA35E", "#45A291"), labels = c('Day', 'Night')) + 
    scale_fill_manual(values = c( "#c2d6a4", "#669d62","#1e3d14"), breaks = c('Industrial', 'Agricultural', 'Forested')) + 
    theme_classic() + 
    theme(strip.background = element_blank(),
          strip.text.y = element_blank(), 
          legend.position = "top") +
    labs( y = "",
         color = "", 
         fill = "") + 
    facet_grid(rows = vars(factor(PastLandUse, levels=c('Industrial', 'Agricultural', 'Forested'))))
  
  # get axis breaks 
  atx <- c(as.numeric(na.omit(layer_scales(t)$x$break_positions())))
  
  # unscale x axis 
  f <- t + 
    scale_x_continuous(name = "Relative Cooling Effect (\u00B0C)", 
                     breaks = atx,
                     labels = round(atx * sd(temp_indices$cooling) + mean(temp_indices$cooling), 1))
  
  ggsave('graphics/plu_temp_total.png', f, width = 12, height = 10, units = 'in')
  
  return(f)
  
}
