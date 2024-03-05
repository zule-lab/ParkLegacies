create_plu_tree_direct_fr <- function(model_list){
  
  tree_mods <- model_list[c('model_2_L', 'model_2_S', 'model_3_L', 'model_3_S', 'model_4_L')]
  
  names(tree_mods) <- c('Densité moyenne (arbres / mètre carré)', 'Densité moyenne (arbres / mètre carré)', "Taille médiane de l'arbre (DPH cm)", "Taille médiane de l'arbre (DPH cm)", 'Richesse moyenne des espèces')
  
  mod2l <- make_plot(tree_mods[[1]], names(tree_mods[1])) + ggtitle('Grands arbres (>= 5 cm DHP)')
  mod2s <- make_plot(tree_mods[[2]], names(tree_mods[2])) + ggtitle('Petits arbres (< 5 cm DHP)')
  mod3l <- make_plot(tree_mods[[3]], names(tree_mods[3]))
  mod3s <- make_plot(tree_mods[[4]], names(tree_mods[4]))  
  mod4l <- make_plot(tree_mods[[5]], names(tree_mods[5]))
  
  
  theme_transparent <- theme_minimal() +
    theme(plot.background = element_rect(fill = "transparent",colour = NA))
  
  p <- mod2l + mod3l + mod4l + mod2s + mod3s + guide_area() + plot_layout(guides = "collect") & theme_transparent
  
  
  ggsave('graphics/plu_tree_direct_fr.png', p, width = 16, height = 10, units = 'in', bg = "transparent")
  
  return(p)
}

mod2l
ggsave('graphics/test.png', mod2l)


make_plot <- function(model, label){
  
  model %>%
    spread_draws(`b_Intercept`, r_PastLandUse[condition,]) %>%
    mutate(condition_mean = exp(b_Intercept + r_PastLandUse),
           condition = factor(condition, levels=c("Forested", "Agricultural", "Industrial"), labels = c("Agricole", "Forestier", "Industriel"))) %>%
    ggplot(aes(y = condition, x = condition_mean, fill = condition)) +
    scale_fill_manual(values = c( "#1e3d14", "#669d62", "#c2d6a4")) +  
    stat_halfeye() + 
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 16, face = 'bold',
                                    color = "black"),
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 16)) +
    labs(y = "", x = label)  
    
    
}
  
