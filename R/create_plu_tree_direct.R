create_plu_tree_direct <- function(model_list, temp_indices){
  
  tree_mods <- model_list[c('model_2_L', 'model_2_S', 'model_3_L', 'model_3_S', 'model_4_L')]
  
  names(tree_mods) <- c('Mean Basal Area (square meter / ha)', 'Mean Basal Area (square meter / ha)', 'Median Tree Size (DBH cm)', 'Median Tree Size (DBH cm)', 'Mean Species Richness (# species)')
  
  mod2l <- make_plot(tree_mods[[1]], temp_indices, names(tree_mods[1])) + ggtitle('Large Trees (>= 5 cm DBH)')
  mod2s <- make_plot(tree_mods[[2]], temp_indices, names(tree_mods[2])) + ggtitle('Small Trees (< 5 cm DBH)') 
  mod3l <- make_plot(tree_mods[[3]], temp_indices, names(tree_mods[3]))
  mod3s <- make_plot(tree_mods[[4]], temp_indices, names(tree_mods[4]))
  mod4l <- make_plot(tree_mods[[5]], temp_indices, names(tree_mods[5]))
  
  
  p <- mod2l + mod3l + mod4l + mod2s + mod3s + guide_area() + plot_layout(guides = "collect")
  
  ggsave('graphics/plu_tree_direct.png', p, width = 12, height = 10, units = 'in')
  
  return(p)
}



make_plot <- function(model, df, label){
  
  response <- colnames(model$data)[1] %>% str_remove("_s")
  
  mod_draws <- model %>%
    emmeans(~ PastLandUse,
            at = list(PastLandUse = c("Industrial", "Agricultural", "Forested")),
            epred = TRUE, re_formula = NA) %>%
    contrast(method = "pairwise") %>%
    gather_emmeans_draws() 
  
  s <- ggplot(mod_draws, aes(x = .value, fill = contrast)) +
    stat_halfeye(alpha = 0.7) +
    scale_fill_manual(values = c( "#1e3d14", "#669d62", "#c2d6a4")) + 
    labs(y = "Density", fill = "") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold',
                                    color = "black")) +
    labs(x = label)
  
  # get axis breaks
  satx <- c(as.numeric(na.omit(layer_scales(s)$x$break_positions())))
  
  # unscale x axis
  s +
    scale_x_continuous(name = label,
                       breaks = satx,
                       labels = (round(satx * (sd(pull(df, response), na.rm = T)), 1)))
  
    
    
}
  
