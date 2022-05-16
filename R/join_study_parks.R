join_study_parks <- function(studyparks, mtlparks, osmparks){
  
  gpo <- studyparks %>% 
    rename(Nom = ParkOfficial) %>% 
    inner_join(., mtlparks, by = c("Nom", "OBJECTID"))
  gpo <- gpo[, c(1:5, 15)]
  gpo <- rename(gpo, ParkOfficial = Nom)

  mp <- studyparks %>% 
    rename(Nom = ParkOfficial) %>% 
    dplyr::filter(Nom != "Grand parc de l'Ouest") %>% 
    left_join(., mtlparks, by = "Nom")
  mp <- st_as_sf(mp)
  mp <- mp[-c(19, 23, 29, 34, 52), c(1:5, 16)] # remove extra polygons
  mp <- rename(mp, ParkOfficial = Nom,
             OBJECTID = OBJECTID.x)

  studyparks <- st_as_sf(rbind(gpo, mp))
  
  studyparksu <- studyparks %>% 
    group_by(Name) %>%
    summarize(geometry = st_union(geometry), 
              PastLandUse = dplyr::first(PastLandUse))
  
  return(studyparksu)
}