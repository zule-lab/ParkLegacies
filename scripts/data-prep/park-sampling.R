target_park_sampling <- c(
  
  tar_file_read(
    cc_2019,
    'input/canopy/660_IndiceCanopee_2019.tif',
    read_stars(!!.x)
  ),
  
  # random sample of potential sampling points
  tar_target(
    sp_pts, 
    sample_points(cc_2019, full_study_parks)
  ),
  
  tar_target(
    sp_pts_buff,
    st_buffer(sp_pts, dist = 20)
  ),
  
  tar_target(
    soil_core_pts,
    soil_cores(sp_pts_buff)
  ),
  
  tar_target(
    worm_pts,
    worm_samples(sp_pts_buff)
  ),
  
  # manually select locations for temperature sensors deployed in 2022 (data lost)
  tar_target(
    temp_sp_pts_2022,
    sp_pts_temp(field_sp_pts, 2022)
  ),
  
  # manually select locations for temperature sensors deployed in 2023
  tar_target(
    temp_sp_pts_2023,
    sp_pts_temp(field_sp_pts, 2023)
  ),
  
  # save
  tar_target(
    save_sp_pts, 
    st_write(sp_pts, "output/park_sampling_points.kml", append = F)
  ),
  
  # get sampling points actually used in field
  tar_target(
    field_sp_pts,
    sp_pts_field('input/sampling_points/gps_sppts.gpkg', 'input/sampling_points/MP_points.kml')
  )
  
)
