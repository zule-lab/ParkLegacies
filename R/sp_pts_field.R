sp_pts_field <- function(gps_sp_pts, mp_sp_pts) { 
  
  # GPS points
  gps_sp_pts <- gps_sp_pts %>%
    select(Name, geom) %>%
    rename(PlotID = Name, 
           geometry = geom) %>%
    mutate(PlotID = replace(PlotID, PlotID == 'AGR6-L0W04', 'AGR6-LOW04'),
           PlotID = replace(PlotID, PlotID == 'IND11-LOW4', 'IND11-LOW04'))
  
  # Points recorded by phone (because GPS was dead, unavailable, teams were split up, etc.)
  mp_sp_pts <- mp_sp_pts %>%
    select(Name, geometry) %>%
    rename(PlotID = Name)
  
  # identify which points are not present in GPS dataset (default to GPS because more accurate)
  anti <- anti_join(st_set_geometry(mp_sp_pts, NULL), gps_sp_pts, by = "PlotID") %>%
    inner_join(., mp_sp_pts, by = "PlotID") %>%
    st_as_sf()
  
  # bind missing data points to GPS dataset for full sampling points
  full <- rbind(gps_sp_pts, anti)
  
  return(full)
  
  
  }