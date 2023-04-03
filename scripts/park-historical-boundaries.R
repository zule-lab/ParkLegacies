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
    ag_bound,
    bound_ag(ag_bound_file)
    ),
  
  tar_target(
    for_bound_file,
    'input/mon_historical_for_boundaries.gpkg',
    format = "file"
  ),
  
  tar_target(
    for_bound,
    read_sf(for_bound_file)
  ),
  
  tar_target(
    mix_bound_file,
    'input/mon_historical_mix_boundaries.gpkg',
    format = "file"
  ),
  
  tar_target(
    mix_bound,
    read_sf(mix_bound_file)
  ),
  
  # transform data to match other layers 
  tar_target(
    ind_trans, 
    st_transform(ind_bound, 4326)
    ), 
  
  # intersect historical industrial sites to park boundaries to isolate relevant sections 
  tar_target(
    ind_parks_raw, 
    st_intersection(st_make_valid(offic_study_parks), st_make_valid(ind_trans))
    ),
  
  # union polygons from same park and remove indeterminate locations 
  tar_target(
    ind_parks, 
    parks_ind(ind_parks_raw)
  ),
  
  tar_target(
    mix_parks, 
    parks_mix(mix_bound)
  ),
  
  # create dataframe of all hand drawn boundaries
  tar_target(
    all_bound, 
    rbind(ag_bound, for_bound, ind_parks, mix_parks)
  ),
  
  tar_target(
    full_study_parks, 
    join_all_bounds(all_bound, offic_study_parks)
  ),
  
  tar_target(
    save_all,
    write_sf(full_study_parks, "output/study_parks_spatial.gpkg")
  )

)
