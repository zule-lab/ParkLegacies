indices_plots_trees <- function(trees_clean, field_sp_pts, full_study_parks){
  
  
  # fix typos 
  field_sp_pts <- field_sp_pts %>% 
    mutate(PlotID = case_when(PlotID == "ARG3-MED06" ~ "AGR3-MED06",
                              .default = PlotID))
  
  
  
  trees_clean$Date <- as.Date(trees_clean$Date, "%m/%d/%Y")
  
  # Large Trees -------------------------------------------------------------
  large_div <- calculate_div_plots(trees_clean, 'DBHCalc >= 5', "L", 800)
  
  
  # Small Trees -------------------------------------------------------------
  # plots before Jul 18, 2022 did not have mini plots for small trees 
  # only going to use plots Jul 18 and later for diversity 
  small_nondiv <- trees_clean %>% 
    filter(Date < '2022-07-18' & DBHCalc < 5) %>%
    mutate(Area = 800,
           Basal_m = (pi*DBHCalc^2)/40000 ) %>% 
    group_by(PlotID) %>% 
    reframe(Park = trimws(Park),
            BasalArea_S = sum(Basal_m)/0.08,
            Abundance_S = n(),
            DBH_med_S = median(DBHCalc),
            DBH_sd_S = sd(DBHCalc),
            Dens_S = Abundance_S/Area) %>% 
    ungroup() %>% 
    unique()
  
  #TODO: current bug in iNEXT that I don't know how to get around??
  #small_div <- calculate_div(trees_clean %>% filter(Date >= '2022-07-18'), 'DBHCalc <= 5', "S", 0.005)
  
  # temporary fix to iNEXT issues 
  small_mini <- trees_clean %>% 
    filter(Date >= '2022-07-18' & DBHCalc < 5) %>% 
    mutate(Area = 50,
           Basal_m = (pi*DBHCalc^2)/40000 ) %>% 
    group_by(PlotID) %>% 
    reframe(Park = trimws(Park),
           BasalArea_S = sum(Basal_m)/0.005,# units are m2/ha
           Abundance_S = n(),
           DBH_med_S = median(DBHCalc),
           DBH_sd_S = sd(DBHCalc),
           Dens_S = Abundance_S/Area) %>% 
    ungroup() %>%
    unique()
  
  # all small trees
  small_trees <- rbind(small_nondiv, small_mini)
  
  
  # All Trees ---------------------------------------------------------------
  # join measurements from small trees and large trees together
  all_trees <- full_join(large_div, small_trees, by = c("PlotID")) %>%
    rename(Park = Park.x) %>% 
    select(-Park.y) %>% 
    mutate(Park = case_when(str_detect(PlotID, "AGR7") == T ~ "Parc Benny",
                            str_detect(PlotID, "IND12") == T ~ "Pointe-aux-Prairies",
                            .default = Park)) %>% 
    mutate(BasalArea_L = ifelse(is.na(BasalArea_L), 0, BasalArea_L),
           BasalArea_S = ifelse(is.na(BasalArea_S), 0, BasalArea_S),
           SR_L = ifelse(is.na(SR_L), 0, SR_L),
           PastLandUse = case_when(str_detect(PlotID, "AGR") == T ~ 'Agricultural',
                                   str_detect(PlotID, "FOR") == T ~ 'Forested',
                                   str_detect(PlotID, "IND") == T ~ 'Industrial'))
    
    
  
  
  # create full dataset w study park info (geom and age)
  full <- full_study_parks %>%
    st_make_valid() %>% 
    rename(Park = Name) %>% 
    inner_join(all_trees, by = c("Park", "PastLandUse")) %>%
    mutate(Park = case_when(Park == "Père-Marquette" ~ "Père-Marquette ",
                            .default = Park)) %>% 
    distinct()
  
  return(full)
  
}


calculate_div_plots <- function(trees_clean, desDBH, suffix, area) {
  
  abundance <- trees_clean %>%
    filter(eval(rlang::parse_expr(desDBH))) %>% 
    mutate(Basal_m = (pi*DBHCalc^2)/40000 ) %>%
    group_by(PlotID) %>% 
    reframe(Park = trimws(Park),
            Abundance_L = n(),
            DBH_med_L = median(DBHCalc),
            DBH_sd_L = sd(DBHCalc),
            BasalArea_L = sum(Basal_m)/0.08) %>% 
    ungroup() %>%
    unique()
  
  div <- trees_clean %>% 
    filter(eval(rlang::parse_expr(desDBH))) %>% 
    unite(scientific_name, c("Genus", "Species"), sep = " ") %>% 
    group_by(PlotID, scientific_name) %>% 
    summarize() %>% 
    filter(scientific_name != " ") %>% 
    distinct() %>%
    group_by(PlotID) %>% 
    summarize(SR_L = n())
  
  full <- full_join(abundance, div)
  
  return(full)
}

  
