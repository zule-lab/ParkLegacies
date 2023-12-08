create_plu_temp_total <- function(model_list, temp_indices){

  model_5 <- model_list[[7]]
  
  t <- model_5 %>%
    epred_draws(expand_grid(PastLandUse = c('Agricultural', 'Forested', 'Industrial'),
                            Age_s = 0,
                            tod = c('day', 'night')),
                re_formula = ~ (1 | tod)) %>%
    ggplot(aes(x = .epred, fill = PastLandUse, color = tod)) +
    stat_halfeye() +
    scale_color_brewer() + 
    scale_fill_manual(values = c( "#c2d6a4", "#669d62","#1e3d14"), breaks = c('Industrial', 'Agricultural', 'Forested')) + 
    theme_classic() + 
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    labs(x = "Cooling Effect - Mean Cooling Effect", y = "",
         color = "", 
         fill = "") + 
    facet_grid(rows = vars(factor(PastLandUse, levels=c('Industrial', 'Agricultural', 'Forested'))))
  
  ggsave('graphics/plu_temp_total.png', t, width = 12, height = 10, units = 'in')
  
}
