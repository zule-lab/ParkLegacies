create_plu_temp_total <- function(model_list, temp_indices){
  
  # extract relevant model from list
  model_5 <- model_list[['model_5']]
  
  
  # plot
  t <- model_5 %>%
    epred_draws(expand_grid(PastLandUse = c('Agricultural', 'Forested', 'Industrial'),
                            Age_s = 0,
                            tod = c('day', 'night')),
                re_formula = ~ (1 | tod)) 
  
  plu <- c("Agricole", "Forestier", "Industriel")
  names(plu) <- c("Agricultural", "Forested", "Industrial")
  
  t <- t %>% ggplot(aes(x = .epred, fill = tod)) +
    stat_halfeye(alpha = 0.7) +
    scale_fill_manual(values = c("#CFA35E", "#45A291"), labels = c('Jour', 'Nuit')) + 
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
    facet_wrap(~ PastLandUse, ncol = 1, labeller = labeller(PastLandUse = plu))
    #facet_grid(rows = vars(factor(PastLandUse, levels=c('Industrial', 'Agricultural', 'Forested'))))
  
  # get axis breaks 
  atx <- c(as.numeric(na.omit(layer_scales(t)$x$break_positions())))
  
  # unscale x axis 
  f <- t + 
    scale_x_continuous(name = "Effet relatif de refroidissement (\u00B0C)", 
                     breaks = atx,
                     labels = round(atx * sd(temp_indices$cooling) + mean(temp_indices$cooling), 1))
  
  ggsave('graphics/plu_temp_total.png', f, width = 12, height = 10, units = 'in')
  
  return(f)
  
}
