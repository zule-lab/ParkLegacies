target_redo_canopy <- c(

  tar_file_read(
    cc_2021,
    'input/canopy/660_IndiceCanopee_2021.tif',
    read_stars(!!.x)
  ),
  
  tar_target(
    field_cc,
    canopy_cover(sp_pts, field_sp_pts, cc_2021)
  )
  
  
)