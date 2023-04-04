target_park_sampling <- c(
  
  tar_file_read(
    cc_2019,
    'input/canopy/660_IndiceCanopee_2019.tif',
    read_stars(!!.x)
  ),
  
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
  
  # save
  tar_target(
    save_sp_pts, 
    st_write(sp_pts, "output/park_sampling_points.kml", append = F)
  ),
  
  tar_target(
    save_soil_core_pts,
    st_write(st_transform(soil_core_pts, 4326), "output/soil_core_sampling_points.kml", append = F)
  ),
  
  tar_target(
    save_earthworm_pts,
    st_write(worm_pts, "output/earthworm_sampling_points.kml", append = F)
  )
  
)
