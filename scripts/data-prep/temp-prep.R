target_temp_prep <- c(
  
  tar_file_read(
    sensor_pts,
    'input/sampling_points/temp_sensors_parks.csv',
    read_sf(!!.x)
  ),
  
  tar_file_read(
    sensor_con_pts,
    'input/sampling_points/temp_sensors_controls.csv',
    read_sf(!!.x, crs = 4326)
  ),
  
  tar_files(
    temp_files,
    dir('input/temperature', full.names = TRUE)
  ),
  
  tar_target(
    temp_dfs, 
    read.csv(temp_files) %>%
      mutate(plot_id = str_extract(basename(xfun::sans_ext(temp_files)), "[^_]+")),
    pattern = map(temp_files)
  ),
  
  tar_target(
    temp_clean,
    clean_temp(temp_dfs, trees_clean)
  ),
  
  tar_target(
    temp_indices,
    indices_temp(temp_clean, sensor_pts, sensor_con_pts)
  )
  
)
