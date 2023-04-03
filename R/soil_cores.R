soil_cores <- function(sp_pts_buff){
  
  # split sample buffers into four polygons to randomly sample
  soil_grid <- pmap_dfr(sp_pts_buff, function(PlotID, geometry, ...) {
    geometry <- st_make_grid(geometry, n = c(2,2))
    x <- st_as_sf(geometry, crs = 4326) %>% 
      mutate(PlotID = PlotID)})
  
  # randomly sample one point within each of the four polygons / buffer
  soil_core_pts <- st_sample(soil_grid, size = c(1,1), type = "random", by_polygon = T, exact = T) %>% 
    st_as_sf() %>% 
    mutate(PlotID = soil_grid$PlotID)
  
  return(soil_core_pts)
  
}