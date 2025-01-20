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
    mutate(popdens_diff = popdens - mtlpopdens,
           income_diff = medinc - mtlincome,
           sideho_diff = sidehop - mtlside,
           vismin_diff = visminp - mtlvismin,
           recimmp_diff = recimmp - mtlrecimm, 
           edubacp_diff = edubacp - mtledu) %>% 
    group_by(PastLandUse) %>% 
    summarize(popdens = paste0(round(mean(popdens_diff), 0), " +/- ", round(sd(popdens_diff), 0)),
              income = paste0(round(mean(income_diff), 0), " +/- ", round(sd(income_diff), 0), ")"),
              sideho = paste0(round(mean(sideho_diff)*100, 1), " +/- ",  round(sd(sideho_diff)*100, 1)),
              vismin = paste0(round(mean(vismin_diff)*100, 1), " +/- ",  round(sd(vismin_diff)*100, 1)),
              recimm = paste0(round(mean(recimmp_diff)*100, 1), " +/- ",  round(sd(recimmp_diff)*100, 1)),
              edu = paste0(round(mean(edubacp_diff)*100, 1), " +/- ",  round(sd(edubacp_diff)*100, 1))
              ) %>% 
    mutate(PastLandUse = factor(PastLandUse, levels=c("Industrial", "Agricultural", "Forested"))) %>% 
    arrange(PastLandUse) %>% 
    gt() %>% 
    cols_label(
      PastLandUse = "**Past Land Use**",
      popdens = "**Population Density <br>(per km<sup>2)**",
      income = "**Median Total Income <br>($)**",
      sideho = "**Single Detached Homes <br>(% residents)**",
      vismin = "**Visible Minorities <br>(% residents)**", 
      recimm = "**Recent Immigrants <br>(% residents)**",
      edu = "**Attained a Bachelor's Degree or Higher <br>(% residents)**",
      .fn = md) %>% 
    tab_header(title = "Table 1. Differences in socioeconomic variables between neighbourhoods surrounding each park type and Montréal's averages. Data was obtained from the 2021 Canadian census (CITE).
                 Dissemination areas within walking distance (1000 m, cite) of each park were intersected and spatially weighted
                 to calculate the average population density, median total income, percentage of single detached homes, 
                 percentage of visible minorities, percentage of recent immigrants (arrived between 2016-2021), and percentage of 
                 residents who have received a bachelor's degree or higher. Means were then calculated for each past land-use type (n = 11 for each).
                 We then subtracted the island of Montréal's mean values for each variable for each group. The table is showing the difference 
                 between the group and the island of Montréal average, with positive values indicating higher than expected and negative indicating lower than expected.") %>% 
    gtsave('output/table_1.docx')
  
  return(df)
  
  
}