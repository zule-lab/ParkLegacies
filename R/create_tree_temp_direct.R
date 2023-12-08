create_tree_temp_direct <- function(model_list, temp_indices){
  
  # formula = cooling ~ 1 + Dens_L_s + DBH_med_L_s + SR_L_s +  Dens_S_s + DBH_med_S_s + (1 | tod) + (1 | Park)
  mod <- model_list[[1]]
  
  # calculate unscaled cooling and add to temp indices
  temp_indices <- temp_indices %>% 
    ungroup() %>% 
    mutate(cooling = ((mean - mean_con) - mean(mean - mean_con)))
  
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
    p <- epred %>% 
      ggplot(aes(x = get(x), y = .epred, color = factor(tod))) +
      stat_lineribbon() +
      scale_fill_brewer(palette = "Greys") +
      scale_color_viridis_d(direction = -1) + 
      theme(legend.position = "none") +
      theme_classic()
    
    # extract plot breaks on x and y axes
    atx <- c(as.numeric(na.omit(layer_scales(p)$x$break_positions())))
    aty <- c(as.numeric(na.omit(layer_scales(p)$y$break_positions())))
    
    # get variable name we are altering (without _s on the end indicating scaled)
    xunsc <- str_sub(x, end = -3)
    
    # extract original variable from dataset
    relvar <- temp_indices %>%
      select({{xunsc}})
    
    # calculate sd and mean of original variable
    sdx <- sd(relvar[[1]], na.rm = T)
    meanx <- mean(relvar[[1]], na.rm = T)
    
    # get x axis label 
    label <- findname(xunsc)
    
    # unscale axis labels for interpretation
    p +
      scale_x_continuous(breaks = atx,
                         labels = round(atx * sdx + meanx, 1)) + 
      scale_y_continuous(name = "Cooling Effect - Mean Cooling Effect", 
                         breaks = aty,
                         labels = round(aty * sd(temp_indices$cooling) + mean(temp_indices$cooling), 1)) + 
      labs(x = label)
    
    
  })
  
  # patchwork a list of figures 
  w <- wrap_plots(figlist)
  
  ggsave('graphics/tree_temp_direct.png', w, width = 12, height = 10, units = c('in'))
  
}





findname <- function(xunsc){
  
  if(xunsc == "Dens_L"){label <- "Mean Large Tree Density (trees / ha)"}
  
  if(xunsc == "DBH_med_L"){label <- "Median Large Tree Size (DBH cm)"}
  
  if(xunsc == "SR_L"){label <- "Mean Large Species Richness (# species)"}
  
  if(xunsc == "Dens_S"){label <- "Mean Small Tree Density (trees / ha)"}
  
  if(xunsc == "DBH_med_S"){label <- "Median Small Tree Size (DBH cm)"}
  
  
}
