clean_census_da <- function(parks, da, census){
  

# spatial -----------------------------------------------------------------

  # transform parks to census projection and make buffer 
  # NOTE: buffers overlap a bunch
  parks_da <- st_transform(parks, st_crs(da)) %>%
    st_buffer(1000) %>%
    st_intersection(da) %>% 
    mutate(DAUID = as.integer(DAUID),
           areaint = st_area(geom)) %>% 
    st_set_geometry(NULL)

  units(parks_da$areaint) <- make_units(km^2) # area of the DA that intersects w the buffer (sq km)

# census ------------------------------------------------------------------
  
  census_da_f <- census %>%
    select(c("ALT_GEO_CODE","CHARACTERISTIC_ID","C1_COUNT_TOTAL")) %>%
    rename(DAUID = "ALT_GEO_CODE",
           sofac = "CHARACTERISTIC_ID",
           sonum = "C1_COUNT_TOTAL") %>%
    full_join(parks_da, by = "DAUID") %>%
    filter(sofac %in% c(1, 6:7, 42:49, 115, 345, 1527, 1534, 1402, 1403, 1683, 1684, 2014, 2024)) 
  
  # 1 = Population 2021
  # 6 = Pop density per sq km
  # 7 = Land area sq km
  # 41 = Total - Occupied private dwellings by structural type of dwelling - 100% data (7)
  # 42 = Single-detached house
  # 43 = Semi-detached house
  # 44 = Row house
  # 45 = Apartment or flat in a duplex
  # 46 = Apartment in a building that has fewer than five storeys
  # 47 = Apartment in a building that has five or more storeys
  # 48 = Other single-attached house
  # 49 = Movable dwelling (9)
  # 115 = Median after-tax income in 2015 among recipients 
  # 345 = Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)
  # 1527 = Total - Immigrant status and period of immigration for the population in private households - 25% sample data (63) -> Immigrants -> 2011-2016 (recent immigrants)
  # 1534 = Immigrants    2011 to 2021 (82)
  # 1402 = Total - Indigenous identity for the population in private households - 25% sample data (44)
  # 1403 = Indigenous identity (45)
  # 1683 = Total - Visible minority for the population in private households - 25% sample data (117)
  # 1684 = Total visible minority population (118)
  # 2014 = Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data (165)
  # 2024 = Bachelor's degree or higher
  # we want percentages for all datasets except median income - need to transform 41:49, 1527, 1403, 1684, 2024
  # start with dwelling types
  
  census_da_w <- census_da_f %>% pivot_wider(names_from = sofac, values_from = sonum)
  
  census_da_r <- census_da_w %>%
    rename(totpop = "1") %>%
    rename(popdens = "6") %>%
    rename(area = "7") %>%
    rename(sideho = "42") %>%
    rename(semhou = "43") %>%
    rename(rowhou = "44") %>%
    rename(aptdup = "45") %>%
    rename(aptbui = "46") %>%
    rename(aptfiv = "47") %>%
    rename(otsiho = "48") %>%
    rename(mvdwel = "49") %>%
    rename(medinc = "115") %>%
    rename(lowinc = "345") %>%
    rename(totimm = "1527") %>%
    rename(recimm = "1534") %>%
    rename(totindig = "1402") %>%
    rename(indig = "1403") %>%
    rename(totvismin = "1683") %>%
    rename(vismin = "1684") %>%
    rename(totedu = "2014") %>%
    rename(edubac = "2024")
  
  census_da_na <- census_da_r %>% 
    drop_na(Name)  %>%
    filter(area > 0) %>%
    mutate(da = as.factor(DAUID)) %>%
    mutate(across(c(totpop:edubac), ~as.numeric(.))) %>%
    select(-c(DGUID, LANDAREA, PRUID, DAUID))
  
  can_cen <- setDT(census_da_na)
  
  can_cen_p <- can_cen[, c(paste0(names(can_cen[,c('sideho', 'semhou', 'rowhou', 'aptdup', 'aptbui', 'aptfiv', 'otsiho', 'mvdwel')]), "p")) := lapply(.SD, function(x) x / sum(.SD)), by=1:nrow(can_cen), .SDcols = c('sideho', 'semhou', 'rowhou', 'aptdup', 'aptbui', 'aptfiv', 'otsiho', 'mvdwel')]
  
  ## population percentages
  can_cen_pp <- can_cen_p[, recimmp := recimm/totimm
  ][, indigp := indig/totindig
  ][, visminp := vismin/totvismin
  ][, edubacp := edubac/totedu]
  

# union -------------------------------------------------------------------

  
  # need to calculate the area of the DA that is within the neigbourhood (areaint)
  park_cen_pop <- can_cen_pp %>%
    # to calculate the approximate population within the neighbourhood bounds (assuming equal density throughout the DA)
    # divide the intersected area/total area of DA and multiply the population by that 
    # can then use this population as weight for weighted means
    mutate(popwithin = (as.numeric(areaint)/as.numeric(area))*as.numeric(totpop)) %>% 
    select(c("Name","PastLandUse","da","totpop", "popwithin", "popdens", "area", "areaint", "sidehop","aptfivp","semhoup","rowhoup","aptdupp","aptbuip","otsihop","mvdwelp",
             "medinc", "lowinc", "recimmp", "indigp", "visminp", "edubacp"))
  
  # population weighted mean
  park_cen <- park_cen_pop %>%
    group_by(Name, PastLandUse) %>%   
    summarize(DAcount = n(),
              totarea = sum(areaint),
              popdens = weighted.mean(as.numeric(popdens), as.numeric(popwithin), na.rm = T),
              sidehop = weighted.mean(as.numeric(sidehop), as.numeric(popwithin), na.rm = T),
              aptfivp = weighted.mean(as.numeric(aptfivp), as.numeric(popwithin), na.rm = T),
              semhoup = weighted.mean(as.numeric(semhoup), as.numeric(popwithin), na.rm = T),
              rowhoup = weighted.mean(as.numeric(rowhoup), as.numeric(popwithin), na.rm = T),
              aptdupp = weighted.mean(as.numeric(aptdupp), as.numeric(popwithin), na.rm = T),
              aptbuip = weighted.mean(as.numeric(aptbuip), as.numeric(popwithin), na.rm = T),
              otsihop = weighted.mean(as.numeric(otsihop), as.numeric(popwithin), na.rm = T),
              mvdwelp = weighted.mean(as.numeric(mvdwelp), as.numeric(popwithin), na.rm = T),
              medinc = weighted.mean(as.numeric(medinc), as.numeric(popwithin), na.rm = T),
              lowinc = weighted.mean(as.numeric(lowinc), as.numeric(popwithin), na.rm = T),
              recimmp = weighted.mean(as.numeric(recimmp), as.numeric(popwithin), na.rm = T),
              indigp = weighted.mean(as.numeric(indigp), as.numeric(popwithin), na.rm = T),
              visminp = weighted.mean(as.numeric(visminp), as.numeric(popwithin), na.rm = T) ,
              edubacp = weighted.mean(as.numeric(edubacp), as.numeric(popwithin), na.rm = T),
              popwithin = sum(as.numeric(popwithin))
    ) %>%
    distinct(Name, PastLandUse, .keep_all = TRUE)
  
  park_cen$Age <- parks$Age[match(park_cen$Name, parks$Name)]
  
  return(park_cen)
    
  
}