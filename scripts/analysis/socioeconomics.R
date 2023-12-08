targets_socioeconomics <- c(
  
  tar_target(
    socioec_data,
    data_socioec(census)
  ),
  
  tar_target(
    socioec_table,
    socioec_data %>%
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
  )
  
  
  
)
