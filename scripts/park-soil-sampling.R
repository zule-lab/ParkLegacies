park_soil_sampling <- c(
  
  # split sample buffers into four polygons to randomly sample
  tar_target(
    soil_grid,
    pmap_dfr(sp_pts_buff, function(geometry, ...) {
      geometry <- st_make_grid(geometry, n = c(2,2))
      x <- st_as_sf(geometry, crs = 4326)
    })
  ),
  
  # randomly sample one point within each of the four polygons / buffer
  tar_target(
    soil_core_pts,
    st_sample(soil_grid, size = c(1,1), type = "random", by_polygon = T) %>% st_as_sf()
  ),
  
  # randomly sample three points within each buffer for earthworm quadrats
  tar_target(
    worm_pts_geom,
    st_sample(sp_pts_buff, size = c(3,3), type = "random", by_polygon = T) %>% st_as_sf()
  ),
  
  tar_target(
    worm_pts_int,
    st_intersection(worm_pts_geom, sp_pts_buff)
  ),
  
  # create unique ids for earthworm quadrats
  tar_target(
    worm_pts,
    worm_pts_int %>%
      group_by(PlotID) %>%
      mutate(QuadratID = paste0(PlotID, "-", 0, row_number()))
    
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
