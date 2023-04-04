target_analysis_canopy <- c(
  
  tar_file_read(
    gps_sp_pts,
    'input/sampling_points/gps_sppts.gpkg',
    read_sf(!!.x)
  ),
  
  tar_file_read(
    mp_sp_pts,
    'input/sampling_points/MP_points.kml',
    read_sf(!!.x)
  ),
  
  tar_file_read(
    cc_2021,
    'input/canopy/660_IndiceCanopee_2021.tif',
    read_stars(!!.x)
  ),
  
  tar_target(
    field_sp_pts,
    sp_pts_field(gps_sp_pts, mp_sp_pts)
  ),
  
  tar_target(
    field_cc,
    canopy_cover(sp_pts, field_sp_pts, cc_2021)
  )
  
  
)