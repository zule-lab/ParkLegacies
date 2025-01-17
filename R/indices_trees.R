indices_trees <- function(trees_clean, full_study_parks){
  
  trees_clean$Date <- as.Date(trees_clean$Date, "%m/%d/%Y")

# Large Trees -------------------------------------------------------------
  large_div <- calculate_div(trees_clean, 'DBHCalc >= 5', "L", 800)
    
  
# Small Trees -------------------------------------------------------------
  # plots before Jul 18, 2022 did not have mini plots for small trees 
  # only going to use plots Jul 18 and later for diversity 
  small_nondiv <- trees_clean %>% 
    filter(Date < '2022-07-18' & DBHCalc < 5) %>%
    mutate(Area = 800,
           Basal_m = (pi*DBHCalc^2)/40000 ) %>% 
    group_by(PlotID) %>% 
    mutate(BasalPlot = sum(Basal_m)/0.08) %>% 
    ungroup() %>% 
    group_by(Park, PastLandUse) %>% 
    reframe(Park = trimws(Park),
            Abundance_S = n(),
            BasalArea_S = mean(BasalPlot), # units are m2/ha
            DBH_med_S = median(DBHCalc),
            DBH_sd_S = sd(DBHCalc),
            Dens_S = Abundance_S/Area) %>%
    unique()
  
  #TODO: current bug in iNEXT that I don't know how to get around??
  #small_div <- calculate_div(trees_clean %>% filter(Date >= '2022-07-18'), 'DBHCalc <= 5', "S", 0.005)
  
  # temporary fix to iNEXT issues 
  small_mini <- trees_clean %>% 
    filter(Date >= '2022-07-18' & DBHCalc < 5) %>% 
    mutate(Area = 50,
           Basal_m = (pi*DBHCalc^2)/40000 ) %>% 
    group_by(PlotID) %>% 
    mutate(BasalPlot = sum(Basal_m)/0.005) %>% 
    ungroup() %>% 
    group_by(Park, PastLandUse) %>% 
    reframe(Park = trimws(Park),
            Abundance_S = n(),
            BasalArea_S = mean(BasalPlot), # units are m2/ha
            DBH_med_S = median(DBHCalc),
            DBH_sd_S = sd(DBHCalc),
            Dens_S = Abundance_S/Area) %>% 
    unique()
  
  # all small trees
  small_trees <- rbind(small_nondiv, small_mini)
  

# All Trees ---------------------------------------------------------------
  # join measurements from small trees and large trees together
  all_trees <- left_join(large_div, small_trees, by = c("Park", "PastLandUse"))
  
  
  # create full dataset w study park info (geom and age)
  full <- full_study_parks %>%
    rename(Park = Name) %>% 
    inner_join(all_trees, by = c("Park", "PastLandUse")) %>%
    mutate(Park = case_when(Park == "Père-Marquette" ~ "Père-Marquette ",
                            .default = Park))
  
  return(full)
  
}


calculate_div <- function(trees_clean, desDBH, suffix, area) {
  
  abundance <- trees_clean %>%
    filter(eval(rlang::parse_expr(desDBH))) %>%
    mutate(Basal_m = (pi*DBHCalc^2)/40000 ) %>%
    group_by(PlotID) %>% 
    mutate(BasalPlot = sum(Basal_m)/0.08) %>% 
    ungroup() %>% 
    group_by(Park, PastLandUse) %>% 
    reframe(Park = trimws(Park),
            Abundance_L = n(),
            BasalArea_L = mean(BasalPlot), # units are m2/ha
            DBH_med_L = median(DBHCalc),
            DBH_sd_L = sd(DBHCalc)) %>%
    unique()
  
  
  names <- trees_clean %>% 
    group_by(Park, PastLandUse) %>%
    summarize() %>% 
    mutate(Names = paste0(trimws(Park), "_", PastLandUse))
  
  
  trees_list_inext <- trees_clean %>%
    filter(eval(rlang::parse_expr(desDBH))) %>%
    group_by(Park, PastLandUse) %>%
    group_map(~ group_by(.x, PlotID, SpCode) %>% 
                tally() %>% 
                pivot_wider(id_cols = SpCode, names_from = PlotID, values_from = n, values_fill = 0) %>%
                mutate_if(is.numeric, ~1 * (. != 0)) %>%
                column_to_rownames("SpCode")) %>%
    setNames(unique(names$Names))
  
  
  out <- iNEXT(trees_list_inext, datatype = "incidence_raw", q=0)
  
  div <- estimateD(trees_list_inext, datatype="incidence_raw",
                   base="coverage", level=min(out$DataInfo$SC), conf=0.95)
  
  new_columns <- c(paste0("SR_", suffix), 
                   paste0("SR_", suffix, "_LCL"),
                   paste0("SR_", suffix, "_UCL"), 
                   paste0("Shannon_", suffix),
                   paste0("Shannon_", suffix, "_LCL"),
                   paste0("Shannon_", suffix, "_UCL"))
  
  old_columns <- c('qD_SR',
                   'qD.LCL_SR',
                   'qD.UCL_SR',
                   'qD_Shannon',
                   'qD.LCL_Shannon',
                   'qD.UCL_Shannon')
  
  div_w <- div %>%
    filter(Order.q != 2) %>%
    mutate(Indices = case_when(Order.q == 0 ~ 'SR',
                               Order.q == 1 ~ 'Shannon')) %>%
    pivot_wider(id_cols = c(Assemblage), 
                names_from = Indices, 
                values_from = c(qD, qD.LCL, qD.UCL)) %>%
    rename(Code = Assemblage) %>%
    rename_with(~ new_columns, all_of(old_columns)) %>%
    mutate(Park = sub("_.*", "", Code),
           PastLandUse = sub(".*_", "", Code),
           Area = area) %>%
    full_join(abundance, by = c('Park', 'PastLandUse')) %>%
    mutate(!!paste0('Dens_', suffix) := Abundance_L/Area)
    
    return(div_w)
}
