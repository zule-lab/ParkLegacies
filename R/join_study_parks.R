join_study_parks <- function(raw_study_parks, city_parks){
  
  
  ## Transform municipal polygons to match osm
  city_parks_trans <- st_transform(city_parks, 4326)
  
  ## Combine all parks
  # select individual polygons from the Grand Parc de l'Ouest
  gpo <- raw_study_parks %>% 
    rename(Nom = ParkOfficial) %>% 
    inner_join(., city_parks_trans, by = c("Nom")) %>% 
    filter(OBJECTID.y == 5628 & Name == "Cap Saint-Jacques" | 
             OBJECTID.y == 5678 & Name == "Ile-Bizard" | 
             OBJECTID.y == 5613 & Name == "L'Anse-À-L'Orme") %>% 
    select(c(Nom, Name, OBJECTID.y, Established, PastLandUse,  geometry)) %>% 
    rename(OBJECTID = OBJECTID.y,
           ParkOfficial = Nom)

  mp <- raw_study_parks %>% 
    rename(Nom = ParkOfficial) %>% 
    dplyr::filter(Nom != "Grand parc de l'Ouest") %>% 
    left_join(., city_parks_trans, by = "Nom")
  
  mp_f <- st_as_sf(mp) %>% 
    filter(NUM_INDEX != '2448-000' & NUM_INDEX != '2484-000' & NUM_INDEX != '0822-000' & NUM_INDEX != '1225-000' & 
             NUM_INDEX != '2018-000' & NUM_INDEX != '0391-000' & 
             NUM_INDEX != '0956-000' ) %>% # remove extra polygons
    rename(ParkOfficial = Nom,
           OBJECTID = OBJECTID.x) %>%
    select(c(ParkOfficial, Name, OBJECTID, Established, PastLandUse, geometry))

   studyparks <- st_as_sf(rbind(gpo, mp_f))
  
  studyparksu <- studyparks %>% 
    group_by(Name) %>%
    summarize(geometry = st_union(geometry), 
              PastLandUse = dplyr::first(PastLandUse))
  
  return(studyparksu)
}