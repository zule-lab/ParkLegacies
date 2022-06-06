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
    soil_pts_geom,
    st_sample(soil_grid, size = c(1,1), type = "random", by_polygon = T) %>% st_as_sf()
  ),
  
  # get park name and historical land use for each point
  tar_target(
    soil_pts_int, 
    st_intersection(st_transform(soil_pts_geom, st_crs(sample_grid)), sample_grid)
  ),
  
  # create unique IDs
  tar_target(
    soil_pts,
    soil_pts_int %>% 
      mutate(ID = paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', Name, perl = TRUE), substr(PastLandUse, 1, 3), row_number()))
  ),
  
  # save
  tar_target(
    save_soil_pts,
    st_write(st_transform(soil_pts, 4326), "output/park_soil_sampling_points.kml")
  )
  
)
