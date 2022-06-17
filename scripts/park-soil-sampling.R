park_soil_sampling <- c(
  
  # split sample buffers into four polygons to randomly sample
  tar_target(
    soil_grid,
    pmap_dfr(sp_pts_buff, function(PlotID, geometry, ...) {
      geometry <- st_make_grid(geometry, n = c(2,2))
      x <- st_as_sf(geometry, crs = 4326) %>% 
        mutate(PlotID = PlotID)
    }) 
  ),
  
  # randomly sample one point within each of the four polygons / buffer
  tar_target(
    soil_core_pts,
    st_sample(soil_grid, size = c(1,1), type = "random", by_polygon = T, exact = T) %>% 
      st_as_sf() %>% 
      mutate(PlotID = soil_grid$PlotID)
  ),
  
  # randomly sample three points within each buffer for earthworm quadrats
  tar_target(
    worm_pts_geom,
    st_sample(sp_pts_buff, size = c(3,3), type = "random", by_polygon = T) %>% 
      st_as_sf() %>%
      mutate(GroupID = rep(seq_len(nrow(.)/3), each=3))
  ),
  
  # create unique ids for earthworm quadrats
  tar_target(
    worm_pts,
    sp_pts_buff %>% 
      ungroup() %>% 
      mutate(GroupID = row_number()) %>% 
      st_drop_geometry() %>% 
      left_join(worm_pts_geom, ., by = "GroupID") %>% 
      group_by(PlotID) %>% 
      mutate(QuadratID = paste0(PlotID, "-", 0, row_number())) %>%
      select(-GroupID)
    
  ),
  
  # save
  tar_target(
    save_soil_core_pts,
    st_write(st_transform(soil_core_pts, 4326), "output/soil_core_sampling_points.kml", append = F)
  ),
  
  tar_target(
    save_earthworm_pts,
    st_write(worm_pts, "output/earthworm_sampling_points.kml", append = F)
  )
  
)
