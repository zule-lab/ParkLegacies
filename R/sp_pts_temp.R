sp_pts_temp <- function(field_sp_pts, year){
  

  # manual input of temperature sensor sites
  lst_pts <- list_sp_pts(year)
  
  df_pts <- as.data.frame(lst_pts) 
  colnames(df_pts) <- 'PlotID'
  
  # inner join with full sampling point dataframe
  temp_pts <- inner_join(field_sp_pts, df_pts) %>%
    distinct(PlotID, .keep_all = TRUE)   # two AGR3-LOW03, select one


    
}

list_sp_pts <- function(year){
  
  if (year == 2022){
    l <- c('AGR1-LOW01', 'AGR1-MED02', 'AGR1-HIGH01', 'AGR2-LOW04', 'AGR2-MED04', 'AGR2-HIGH03',
             'AGR3-LOW03', 'AGR3-MED05', 'AGR3-HIGH04', 'AGR4-LOW05', 'AGR4-MED05', 'AGR5-LOW03', 
             'AGR5-MED04', 'AGR5-HIGH05', 'AGR5-HIGH06', 'AGR6-LOW04', 'AGR6-MED03', 'AGR6-HIGH03',
             'AGR7-LOW03', 'AGR7-MED04', 'AGR7-HIGH02', 'AGR8-LOW01', 'AGR8-MED04', 'AGR8-HIGH05',
             'AGR9-LOW05', 'AGR9-LOW06', 'AGR9-HIGH03', 'AGR10-MED02', 'AGR10-HIGH01', 'AGR11-LOW02',
             'AGR11-MED04', 'AGR11-HIGH06',
             'FOR1-LOW01', 'FOR1-MED02', 'FOR1-HIGH01', 'FOR2-LOW04', 'FOR2-MED01', 'FOR2-HIGH01',
             'FOR3-LOW03', 'FOR3-MED03', 'FOR3-HIGH06', 'FOR4-LOW04', 'FOR4-MED02', 'FOR4-HIGH04',
             'FOR5-LOW01', 'FOR5-MED04', 'FOR5-HIGH02', 'FOR6-LOW02', 'FOR6-MED05', 'FOR6-HIGH02',
             'FOR7-LOW06', 'FOR7-MED06', 'FOR7-HIGH01', 'FOR8-LOW03', 'FOR8-MED01', 'FOR8-HIGH03',
             'FOR9-LOW01', 'FOR9-MED03', 'FOR9-HIGH03', 'FOR10-MED05', 'FOR10-HIGH02', 'FOR11-LOW01',
             'FOR11-MED03', 'FOR11-HIGH06',
             'IND1-LOW04', 'IND1-MED02', 'IND1-HIGH01', 'IND2-LOW01', 'IND2-MED04', 'IND2-HIGH04',
             'IND3-LOW02', 'IND3-MED02', 'IND3-HIGH03', 'IND5-LOW05', 'IND5-MED04', 'IND5-HIGH01',
             'IND6-LOW03', 'IND6-MED01', 'IND6-HIGH03', 'IND7-LOW04', 'IND7-MED02', 'IND7-HIGH04',
             'IND8-LOW05', 'IND8-MED01', 'IND8-HIGH03', 'IND9-LOW04', 'IND9-MED03', 'IND9-HIGH02',
             'IND11-LOW04', 'IND11-MED06', 'IND11-HIGH03', 'IND12-LOW02', 'IND12-MED01', 'IND12-HIGH03',
             'IND13-LOW04', 'IND13-MED01', 'IND13-HIGH06')
    
    return(l)
  }
  
  else if (year == 2023){
    
    l <- c('AGR1-LOW01', 'AGR1-MED02', 'AGR1-HIGH01', 'AGR2-LOW04', 'AGR2-MED04', 'AGR2-HIGH03',
           'AGR3-LOW03', 'AGR3-MED05', 'AGR3-HIGH04', 'AGR4-LOW01', 'AGR4-MED06', 'AGR4-HIGH03',  
           'AGR5-LOW06', 'AGR5-MED04', 'AGR5-HIGH06', 'AGR6-LOW04', 'AGR6-MED03', 'AGR6-HIGH03',
           'AGR7-LOW05', 'AGR7-MED02', 'AGR7-HIGH02', 'AGR8-LOW01', 'AGR8-MED04', 'AGR8-HIGH05',
           'AGR9-LOW06', 'AGR9-MED05', 'AGR9-HIGH03', 'AGR10-MED02', 'AGR10-HIGH01', 'AGR11-LOW02',
           'AGR11-MED04', 'AGR11-HIGH06',
           'FOR1-LOW01', 'FOR1-MED02', 'FOR1-HIGH01', 'FOR2-LOW04', 'FOR2-MED01', 'FOR2-HIGH06',
           'FOR3-LOW03', 'FOR3-MED03', 'FOR3-HIGH06', 'FOR4-LOW04', 'FOR4-MED02', 'FOR4-HIGH04',
           'FOR5-LOW01', 'FOR5-MED04', 'FOR5-HIGH02', 'FOR6-LOW02', 'FOR6-MED01', 'FOR6-HIGH02',
           'FOR7-LOW06', 'FOR7-MED06', 'FOR7-HIGH01', 'FOR8-LOW03', 'FOR8-MED01', 'FOR8-HIGH03',
           'FOR9-LOW01', 'FOR9-MED03', 'FOR9-HIGH03', 'FOR10-MED05', 'FOR10-HIGH02', 'FOR11-LOW01',
           'FOR11-MED03', 'FOR11-HIGH06',
           'IND1-LOW04', 'IND1-MED02', 'IND1-HIGH01', 'IND2-LOW01', 'IND2-MED04', 'IND2-HIGH02',
           'IND3-LOW02', 'IND3-MED02', 'IND3-HIGH03', 'IND5-LOW05', 'IND5-MED04', 'IND5-HIGH01',
           'IND6-LOW03', 'IND6-MED01', 'IND6-HIGH03', 'IND7-LOW04', 'IND7-MED02', 'IND7-HIGH04',
           'IND8-LOW03', 'IND8-MED01', 'IND8-HIGH03', 'IND9-LOW04', 'IND9-MED03', 'IND9-HIGH02',
           'IND11-LOW04', 'IND11-MED06', 'IND11-HIGH03', 'IND12-LOW02', 'IND12-MED01', 'IND12-HIGH03',
           'IND13-LOW03', 'IND13-MED01', 'IND13-HIGH06')
    
    return(l)
  }
  
  
  else{
    print("error: none of the years matched")
  }
}