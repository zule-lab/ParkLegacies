join_study_parks <- function(raw_study_parks, city_parks){
  
  # set bbox of Montreal
  bbox <- c(xmin = -74.0788,
    ymin = 45.3414,
    xmax = -73.3894,
    ymax = 45.7224)
  
  ##  Get outline of island of Montreal
  # download island boundary in bbox
  mtl <- opq(bbox) %>%
    add_osm_feature(key = 'place', value = 'island') %>%
    osmdata_sf()
  # grab multipolygons (large islands)
  outline_multipolys <- mtl$osm_multipolygons
  # grab polygons (small islands)
  outline_polys <- mtl$osm_polygons
  outline_polys <- st_cast(outline_polys, "MULTIPOLYGON")
  # combine geometries and cast as sf
  outline_allpolys <- st_as_sf(st_union(outline_polys, outline_multipolys))
  
  ## Download park polygons from osm
  # download island boundary in bbox
  parks <- opq(bbox) %>%
    add_osm_feature(key = 'leisure', value = 'park') %>%
    osmdata_sf()
  # grab multipolygons (large parks)
  mpolys <- parks$osm_multipolygons
  mpolys <- st_make_valid(st_cast(mpolys, "POLYGON"))
  # grab polygons (small parks)
  polys <- parks$osm_polygons
  allpolys <- st_union(st_make_valid(polys), st_make_valid(mpolys))
  # clip parks to Montreal island boundary
  wi <- st_within(allpolys, outline_allpolys)
  subwi <- vapply(wi, function(x) length(x) >= 1, TRUE)
  keepp <- allpolys[subwi, ]
  
  
  ## Transform municipal polygons to match osm
  city_parks_trans <- st_transform(city_parks, 4326)
  
  ## Combine all parks
  gpo <- raw_study_parks %>% 
    rename(Nom = ParkOfficial) %>% 
    inner_join(., city_parks_trans, by = c("Nom", "OBJECTID"))
  gpo <- gpo[, c(1:5, 15)]
  gpo <- rename(gpo, ParkOfficial = Nom)

  mp <- raw_study_parks %>% 
    rename(Nom = ParkOfficial) %>% 
    dplyr::filter(Nom != "Grand parc de l'Ouest") %>% 
    left_join(., city_parks_trans, by = "Nom")
  mp <- st_as_sf(mp)
  mp <- mp[-c(19, 20, 22, 23, 24, 29, 30, 34, 35,  52, 54), c(1:5, 16)] # remove extra polygons
  mp <- rename(mp, ParkOfficial = Nom,
             OBJECTID = OBJECTID.x)

  studyparks <- st_as_sf(rbind(gpo, mp))
  
  studyparksu <- studyparks %>% 
    group_by(Name) %>%
    summarize(geometry = st_union(geometry), 
              PastLandUse = dplyr::first(PastLandUse))
  
  return(studyparksu)
}