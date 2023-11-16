data_model <- function(temp_indices, type){
  
  if (type == 'cooling'){
  
  # model is temp ~ past land-use + tree density + tree size + tree diversity 
  data <- temp_indices %>%
    ungroup() %>% 
    mutate(Park = as.factor(Park),
           PastLandUse = as.factor(PastLandUse),
           mean_day_s = scale(mean_day), 
           mean_day_con_s = scale(mean_day_con),
           mean_night_s = scale(mean_night), 
           mean_night_con_s = scale(mean_night_con),
           SR_L_s = scale(SR_L), 
           DBH_med_L_s = scale(DBH_med_L), 
           Dens_L_s = scale(Dens_L)) %>% 
    select(c(Park, date, Age, mean_day_s, mean_day_con_s, mean_night_s, mean_night_con_s, PastLandUse, SR_L_s, DBH_med_L_s, Dens_L_s))
  }
  
  else if (type == 'landuse'){
    data <- temp_indices %>%
      ungroup() %>% 
      mutate(PastLandUse = as.factor(PastLandUse)) %>% 
      select(c(SR_L, DBH_med_L, Dens_L, PastLandUse))
    
  }
  
  
}