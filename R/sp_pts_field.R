sp_pts_field <- function(gps_sp_pts_file, mp_sp_pts_file) { 
  
  gps_sp_pts <- read_sf(gps_sp_pts_file)
  
  mp_sp_pts <- read_sf(mp_sp_pts_file)
  
  
  # GPS points
  gps_sp_pts <- gps_sp_pts %>%
    select(Name, geom) %>%
    rename(PlotID = Name, 
           geometry = geom) %>%
    mutate(PlotID = replace(PlotID, PlotID == 'AGR6-L0W04', 'AGR6-LOW04'),
           PlotID = replace(PlotID, PlotID == 'IND11-LOW4', 'IND11-LOW04'),
           PlotID = replace(PlotID, PlotID == 'IND01-MED01', 'IND1-MED01'))
  
  # Points recorded by phone (because GPS was dead, unavailable, teams were split up, etc.)
  mp_sp_pts <- mp_sp_pts %>%
    select(Name, geometry) %>%
    rename(PlotID = Name) %>%
    mutate(PlotID = replace(PlotID, PlotID == 'IND13-ME02', 'IND13-MED02'))
  
  # identify which points are not present in GPS dataset (default to GPS because more accurate)
  anti <- anti_join(st_set_geometry(mp_sp_pts, NULL), gps_sp_pts, by = "PlotID") %>%
    inner_join(., mp_sp_pts, by = "PlotID") %>%
    st_as_sf()
  
  # take GPS coordinates from datasheets for those that are missing from digital files (n = 4)
  hand <- tibble(PlotID = c("AGR3-MED06",
                            "IND1-HIGH02",
                            "IND2-HIGH07", 
                            "AGR6-LOW06"),
                 Longitude = c(45.516667,
                              45.456767,
                              45.469300,
                              45.475200),
                 Latitude = c(-73.881717,
                               -73.558417,
                               -73.560133,
                               -73.558400)) %>%
    st_as_sf(coords = c("Latitude", "Longitude"), crs = 4326)
  
  
  
  # bind missing data points to GPS dataset for full sampling points
  full <- rbind(gps_sp_pts, anti, hand)
  
  return(full)
  
  
  }