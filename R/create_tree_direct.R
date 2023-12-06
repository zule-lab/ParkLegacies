create_tree_direct <- function(model_list){
  
  tree_mods <- model_list[c('model_2_L', 'model_2_S', 'model_3_L', 'model_3_S', 'model_4_L')]
  
  names(tree_mods) <- c('Mean Tree (>= 5 cm DBH) Density', 'Mean Tree (< 5 cm DBH) Density', 'Median Tree (>= 5 cm DBH) Size', 'Median Tree (< 5 cm DBH) Size', 'Mean Tree (>= 5 cm DBH) Species Richness')
  
  mod2l <- make_plot(tree_mods[[1]], names(tree_mods[1]))
  mod2s <- make_plot(tree_mods[[2]], names(tree_mods[2]))  
  mod3l <- make_plot(tree_mods[[3]], names(tree_mods[3]))
  mod3s <- make_plot(tree_mods[[4]], names(tree_mods[4]))
  mod4l <- make_plot(tree_mods[[5]], names(tree_mods[5]))
  
  
  p <- (mod2l + mod3l + mod4l) / (mod2s + mod3s) + plot_layout(guides = "collect")
  
  ggsave('graphics/tree_direct.png', p, width = 12, height = 10, units = 'in')
  
}



make_plot <- function(model, label){
  
  model %>%
    spread_draws(`b_Intercept`, r_PastLandUse[condition,]) %>%
    mutate(condition_mean = b_Intercept + r_PastLandUse) %>%
    ggplot(aes(y = condition, x = condition_mean)) +
    stat_halfeye() + 
    theme_classic() + 
    labs(y = "", x = label)
}
  
