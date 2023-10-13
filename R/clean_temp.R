clean_temp <- function(temp_dfs, trees_clean){
  
  df <- rbind(temp_dfs) 
  
  names(df) <- make_clean_names(names(df))
  
  df_c <- df %>%
    mutate(temperature_c = round(case_when(is.na(temperature_c) ~ temp,
                                TRUE ~ temperature_c),1),
           date_time = strptime(date_time, format = "%Y-%m-%d %H:%M:%S")) %>%
    select(-c("index", "x", "rh", "temp")) %>%
    drop_na(temperature_c) %>%
    filter(date_time > '2023-06-01 00:00:00' & date_time <'2023-07-31 12:00:00',
           temperature_c < 41)
  
  plots <- trees_clean %>%
    select(Park, PlotID, catcan_2019, catcan_2021) %>% 
    unique()
  
  df_j <- df_c %>% 
    left_join(plots, by = c("plot_id" = "PlotID")) %>%
    mutate(Park = case_when(is.na(Park) ~ "Control",
                            TRUE ~ Park))
  
}