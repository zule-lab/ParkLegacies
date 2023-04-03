target_historical_boundaries <- c(
  
  tar_target(
    ind_bound, 
    download_shp("https://data.montreal.ca/dataset/d332b1ef-95af-42d8-a996-7e451f1c6722/resource/4197dcf3-0f90-4034-b021-85c52ba4c9c2/download/anciennes_carrieres_depot_surface.zip",
               "input/mon_historical_ind_boundaries.zip")
    ),
  
  tar_target(
    ag_bound_file,
    'input/mon_historical_ag_boundaries.gpkg',
    format = "file"
    ),
  
  tar_target(
    for_bound_file,
    'input/mon_historical_for_boundaries.gpkg',
    format = "file"
  ),
  
  tar_target(
    mix_bound_file,
    'input/mon_historical_mix_boundaries.gpkg',
    format = "file"
  ),

  tar_target(
    full_study_parks, 
    join_all_bounds(ind_bound, ag_bound_file, for_bound_file, mix_bound_file, offic_study_parks)
  ),
  
  tar_target(
    save_all,
    write_sf(full_study_parks, "output/study_parks_spatial.gpkg")
  )

)
