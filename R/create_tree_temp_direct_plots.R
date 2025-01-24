create_tree_temp_direct_plots <- function(tree_temp_brms_sample, temp_plots){
  
  
  mod <- tree_temp_brms_sample
  
  # calculate unscaled cooling and add to temp indices
  temp_indices <- temp_plots %>% 
    ungroup() %>% 
    mutate(Park = as.factor(Park),
           PastLandUse = as.factor(PastLandUse),
           tod = as.factor(tod),
           cooling_s = scale(cooling),
           SR_L_s = scale(SR_L), 
           DBH_med_L_s = scale(DBH_med_L), 
           BasalArea_L_s = scale(BasalArea_L),
           BasalArea_S_s = scale(BasalArea_S),
           DBH_med_S_s = scale(DBH_med_S),
           Dens_S_s = scale(Dens_S))
  
  # how to sequence data
  seq_d <- seq(-3, 10, length.out = 1000)
  
  # identify numeric variables in model
  numeric <- names(Filter(is.numeric,mod$data[-1]))
  
  # make a list of figures
  figlist <- lapply(numeric, function(x){
    
    # create dataframe with each numeric variable
    df <- data.frame(matrix(ncol=length(numeric),nrow=0, dimnames=list(NULL, numeric)))
    
    # sequence across each variable from -3 to 3 while holding others at 0
    df %<>%
      summarise_at(x, ~seq_d) %>%
      bind_rows(df, .) %>%
      mutate(across(all_of(numeric), ~ replace(., is.na(.), 0))) %>%
      # expand across two factor levels, day and night
      expand_grid(., tod = c('day', 'night'))
    
    # add model predictions with time of day intercepts
    epred <- mod %>% 
      epred_draws(df, 
                  re_formula = NA)
    
    # plot slopes
    p <- ggplot(data = temp_indices, aes(x = get(x))) +
      geom_jitter(aes(y = cooling_s, color = as.factor(tod)), width = .1, height = 0, alpha = 0.7) +
      stat_lineribbon(aes(x = get(x), y = .epred, color = factor(tod)), data = epred) +
      scale_fill_brewer(palette = "Greys") +
      scale_color_manual(values = c("#CFA35E","#45A291"), labels = c("Day", "Night")) +
      theme_classic()
    
    
    # extract plot breaks on x and y axes
    atx <- c(as.numeric(na.omit(layer_scales(p)$x$break_positions())))
    aty <- c(as.numeric(na.omit(layer_scales(p)$y$break_positions())))
    
    
    # get variable name we are altering (without _s on the end indicating scaled)
    xunsc <- str_sub(x, end = -3)
    
    # extract original variable from dataset
    relvar_s <- temp_indices %>% 
      select({{x}})
    relvar <- temp_indices %>%
      select({{xunsc}})
    
    # calculate sd and mean of original variable
    sdx <- sd(relvar[[1]], na.rm = T)
    meanx <- mean(relvar[[1]], na.rm = T)
    
    # get x axis label 
    label <- findname_plots(xunsc)
    
    # unscale axis labels for interpretation
    p +
      coord_cartesian(xlim = range(relvar_s, na.rm = TRUE), expand = T) + 
      scale_x_continuous(breaks = atx,
                         labels = round(atx * sdx + meanx, 1))  +
      scale_y_continuous(name = "Cooling Effect (\u00B0C)", 
                         breaks = aty,
                         labels = round(aty * sd(temp_indices$cooling) + mean(temp_indices$cooling), 1)) + 
      labs(x = label, colour = NULL, fill = "Credible Interval") + 
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold',
                                      color = "black"))
    
    
  })
  
  # add titles 
  p1 <- figlist[[1]] + ggtitle('Large Trees (>= 5 cm DBH)')
  p4 <- figlist[[4]] + ggtitle('Small Trees (< 5 cm DBH)')
  
  # patchwork a list of figures 
  w <- p1 + figlist[[2]] + figlist[[3]] + 
    p4 + figlist[[5]] + guide_area() + 
    plot_layout(guides ='collect')
  
  ggsave('graphics/tree_temp_direct_plots.png', w, width = 14, height = 12, units = c('in'))
  
  return(w)
  
}




findname_plots <- function(xunsc){
  switch(
    xunsc,
    BasalArea_L = "Tree Basal Area (square meter / ha)",
    DBH_med_L = "Median Size (DBH cm)",
    SR_L = "Mean Species Richness (# species)",
    BasalArea_S = "Tree Basal Area (square meter / ha)",
    DBH_med_S = "Median Tree Size (DBH cm)"
  )
}
