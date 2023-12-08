data_socioec <- function(census){
  
  # montreal mean income extracted from the 2021 Census Profile
  # https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&SearchText=Montreal&DGUIDlist=2021A00052466023&GENDERlist=1,2,3&STATISTIClist=1,4&HEADERlist=0
  mtlpopdens <- 4022.3
  mtlincome <- 37600
  mtlvismin <- 0.376
  mtlrecimm <- 0.058 	
  mtledu <- 0.370
  mtlside <- 0.111
  
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