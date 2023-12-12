create_tree_temp_direct <- function(model_list, temp_indices){
  
  # formula = cooling ~ 1 + Dens_L_s + DBH_med_L_s + SR_L_s +  Dens_S_s + DBH_med_S_s + (1 | tod) + (1 | Park)
  mod <- model_list[[1]]
  
  # calculate unscaled cooling and add to temp indices
  temp_indices <- temp_indices %>% 
    ungroup() %>% 
    mutate(Park = as.factor(Park),
           PastLandUse = as.factor(PastLandUse),
           tod = as.factor(tod),
           cooling = ((mean_con - mean) - mean(mean_con - mean)),
           cooling_s = scale((mean_con - mean) - mean(mean_con - mean)),
           SR_L_s = scale(SR_L), 
           DBH_med_L_s = scale(DBH_med_L), 
           Dens_L_s = scale(Dens_L),
           Dens_S_s = scale(Dens_S),
           DBH_med_S_s = scale(DBH_med_S))
  
  # how to sequence data
  seq_d <- seq(-3, 3, length.out = 500)
  
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
                  re_formula = ~ (1 | tod))
    
    # plot slopes
    p <- ggplot(data = temp_indices, aes(x = get(x))) +
      stat_lineribbon(aes(x = get(x), y = .epred, color = factor(tod)), data = epred) +
      scale_fill_brewer(palette = "Greys") +
      scale_color_manual(values = c("#CFA35E","#45A291")) +
      theme_classic() + 
      theme(legend.position = "top")
      
    
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
    label <- findname(xunsc)
    
    # unscale axis labels for interpretation
    p +
      coord_cartesian(xlim = range(relvar_s)) +
      scale_x_continuous(breaks = atx,
                         labels = round(atx * sdx + meanx, 1)) + 
      scale_y_continuous(name = "Relative Cooling Effect (deg C)", 
                         breaks = aty,
                         labels = round(aty * sd(temp_indices$cooling) + mean(temp_indices$cooling), 1)) + 
      labs(x = label)
    
    
  })
  
  # patchwork a list of figures 
  w <- wrap_plots(figlist, guides = "collect")
  
  ggsave('graphics/tree_temp_direct.png', w, width = 12, height = 10, units = c('in'))
  
}




findname <- function(xunsc){
  switch(
    xunsc,
    Dens_L = "Mean Large Tree Density (trees / ha)",
    DBH_med_L = "Median Large Tree Size (DBH cm)",
    SR_L = "Mean Large Species Richness (# species)",
    Dens_S = "Mean Small Tree Density (trees / ha)",
    DBH_med_S = "Median Small Tree Size (DBH cm)"
  )
}
