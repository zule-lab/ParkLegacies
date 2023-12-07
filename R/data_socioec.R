data_socioec <- function(census){
  
  # montreal mean income extracted from the 2021 Census Profile
  # https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&SearchText=Montreal&DGUIDlist=2021A00052466023&GENDERlist=1,2,3&STATISTIClist=1,4&HEADERlist=0
  mtlpopdens <- 4833.5
  mtlincome <- 36800
  mtlvismin <- 0.388
  mtlrecimm <- 0.06 	
  mtledu <- 0.361
  mtlside <- 0.071
  
  df <- census %>% 
    group_by(PastLandUse) %>% 
    summarize(popdens = mean(popdens) - mtlpopdens,
              income = mean(medinc) - mtlincome,
              sideho = (mean(sidehop) - mtlside)*100,
              vismin = (mean(visminp) - mtlvismin)*100, 
              recimm = (mean(recimmp) - mtlrecimm)*100, 
              edu = (mean(edubacp) - mtledu)*100, 
              ) %>%
    mutate_if(is.numeric, round, 1)
  
  return(df)
  
  
}