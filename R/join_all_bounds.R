#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

join_all_bounds <- function(ind_bound, ag_bound_file, for_bound, mix_bound, official, study_parks_file) {


# Clean -------------------------------------------------------------------

  # transform industrial sites to match other layers
  ind_bound <- st_transform(ind_bound, 4326)
  
  # clean ag read in gpkg
  ag_bound <- ag_bound_file %>% 
    rename(geom = geometry) # rename geometry to match other gpkg layers


# Intersect ---------------------------------------------------------------
  
  # intersect historical industrial sites to park boundaries to isolate relevant sections 
  ind_parks_raw <- st_intersection(st_make_valid(official), st_make_valid(ind_bound))  
  # union polygons from same park and remove indeterminate locations 
  ind_parks <- ind_parks_raw %>% 
    filter(Localisati != "Localisation indéterminée - élément ponctuel approximatf") %>%
    group_by(Name) %>%
    summarise(PastLandUse = "Industrial",
              geom = st_union(geometry))

  mix_parks <- st_make_valid(mix_bound)%>% 
    group_by(Name, PastLandUse) %>%
    summarise(PastLandUse = first(PastLandUse),
                     geom = st_union(geom))

  # create dataframe of all hand drawn boundaries
  all_bound <- rbind(ag_bound, for_bound, ind_parks, mix_parks)
  
  # make full dataset
  missing <- anti_join(official, as.data.frame(all_bound), by = "Name")
  
  missing <- missing %>% 
    summarise(Name = Name,
           PastLandUse = PastLandUse,
           geom = geometry)
  
  all <- rbind(all_bound, missing)
  
  all$Established <- study_parks_file$Established[match(all$Name, study_parks_file$Name)]
  
  all_age <- all %>%
    mutate(Age = (as.integer(format(Sys.Date(), "%Y")) - as.numeric(Established))) %>%
    select(-Established)
  

  return(all_age)
  


}
