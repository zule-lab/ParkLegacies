indices_trees <- function(trees_clean){
  

# Downsample --------------------------------------------------------------

# plots before Jul 18, 2022 did not have mini plots for small trees 
# need to downsample these plots to match mini plots (large plots 0.08 ha, mini-plots 0.005 ha)
# TODO: is dataset missing a plot?
  
  trees_clean$Date <- as.Date(trees_clean$Date, "%m/%d/%Y")
  
  
  plots <- trees_clean %>%
    filter(Date < '2022-07-18') %>%
    group_by(PlotID) %>%
    summarize()
  
  trees_d <- trees_clean %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
    filter(Date < '2022-07-18',
           DBHCalc <= 5) %>%
    group_by(PlotID) %>%
    summarize(nTrees = n(),
              DBHCalc_med = median(DBHCalc),
              DBHCalc_sd = sd(DBHCalc)) %>%
    right_join(plots, by = "PlotID") %>%
    replace(is.na(.), 0)
    
  
  

# Large Trees -------------------------------------------------------------
  
  abundance <- trees_clean %>%
    filter(DBHCalc > 5) %>%
    group_by(PlotID) %>% 
    tally()
  
  
  
  trees_list_inext <- trees_clean %>%
    filter(DBHCalc > 5) %>%
    group_by(PlotID, SpCode) %>%
    tally() %>%
    pivot_wider(id_cols = SpCode, names_from = PlotID, values_from = n, values_fill = 0) %>%
    column_to_rownames("SpCode")
  
  out <- iNEXT(trees_list_inext, datatype = "abundance", q=0)
  
  div <- estimateD(trees_list_inext, datatype="abundance",
                   base="coverage", level=min(out$DataInfo$SC), conf=0.95)
  
  div_w <- div %>%
    filter(Order.q != 2) %>%
    mutate(Indices = case_when(Order.q == 0 ~ 'SR',
                               Order.q == 1 ~ 'Shannon')) %>%
     pivot_wider(id_cols = c(Assemblage), 
                             names_from = Indices, 
                             values_from = c(qD, qD.LCL, qD.UCL)) %>%
    rename(Park = Assemblage,
           SR = qD_SR,
           SR_LCL = qD.LCL_SR,
           SR_UCL = qD.UCL_SR,
           Shannon = qD_Shannon,
           Shannon_LCL = qD.LCL_Shannon,
           Shannon_UCL = qD.UCL_Shannon)

  
}
