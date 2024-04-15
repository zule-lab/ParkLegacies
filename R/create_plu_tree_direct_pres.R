create_plu_tree_direct <- function(model_list){
  
  tree_mods <- model_list[c('model_2_L', 'model_2_S', 'model_3_L', 'model_3_S', 'model_4_L')]
  
  names(tree_mods) <- c('Mean Density (trees / square meter)', 'Mean Density (trees / square meter)', 'Median Size (DBH cm)', 'Median Tree Size (DBH cm)', 'Mean Species Richness (# species)')
  
  mod2l <- make_plot(tree_mods[[1]], names(tree_mods[1])) + ggtitle('Large Trees (>= 5 cm DBH)')
  mod2s <- make_plot(tree_mods[[2]], names(tree_mods[2])) 
  mod3l <- make_plot(tree_mods[[3]], names(tree_mods[3]))
  mod3s <- make_plot(tree_mods[[4]], names(tree_mods[4])) + ggtitle('Small Trees (< 5 cm DBH)') 
  mod4l <- make_plot(tree_mods[[5]], names(tree_mods[5]))
  
  
  p <- mod2l + mod4l + mod3s + plot_layout(guides = "collect")
  
  ggsave('graphics/plu_tree_direct_pres.png', p, width = 16, height = 10, units = 'in')
  
  return(p)
}



make_plot <- function(model, label){
  
  model %>%
    spread_draws(`b_Intercept`, r_PastLandUse[condition,]) %>%
    mutate(condition_mean = exp(b_Intercept + r_PastLandUse),
           condition = factor(condition, levels=c("Forested", "Agricultural", "Industrial"))) %>%
    ggplot(aes(y = condition, x = condition_mean, fill = condition)) +
    scale_fill_manual(values = c( "#1e3d14", "#669d62", "#c2d6a4")) +  
    stat_halfeye() + 
    theme_classic() + 
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold',
                                    color = "black"),
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 16)) +
    labs(y = "", x = label)  
  
  
}

