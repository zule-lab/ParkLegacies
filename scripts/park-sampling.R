target_park_sampling <- c(
  
  tar_target(
    cc_file,
    'input/660_IndiceCanopee_2019.tif',
    format = "file"
  ),
  
  tar_target(
    sp_pts, 
    sample_points(cc_file, full_study_parks)
  ),
  
  tar_target(
    sp_pts_buff,
    st_buffer(sp_pts, dist = 20)
  ),
  
  tar_target(
    save_sp_pts, 
    st_write(sp_pts, "output/park_sampling_points.kml", append = F)
  )
  
)
