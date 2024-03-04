target_park_prep <- c(
  
  # get file path for study park csv 
  tar_file_read(
    study_parks_file,
    'input/studyparks.csv',
    fread(!!.x)
    ),
  
  # download municipal park polygons
  tar_target(
    city_parks, 
    download_shp("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/download/shapefile.zip",
                 "input/mon_park_raw/")
    ),
  
  # download shapefiles of historical industrial sites
  tar_target(
    ind_bound, 
    download_shp("https://data.montreal.ca/dataset/d332b1ef-95af-42d8-a996-7e451f1c6722/resource/4197dcf3-0f90-4034-b021-85c52ba4c9c2/download/anciennes_carrieres_depot_surface.zip",
                 "input/mon_historical_ind_boundaries/")
  ),
  
  # hand-drawn boundaries of agricultural boundaries in parks
  tar_file_read(
    ag_bound,
    'input/park_boundaries/mon_historical_ag_boundaries.gpkg',
    read_sf(!!.x)
  ),
  
  # hand-drawn boundaries of forested boundaries in parks
  tar_file_read(
    for_bound,
    'input/park_boundaries/mon_historical_for_boundaries.gpkg',
    read_sf(!!.x)
  ),
  
  # hand-drawn boundaries for parks with mixed legacy types
  tar_file_read(
    mix_bound,
    'input/park_boundaries/mon_historical_mix_boundaries.gpkg',
    read_sf(!!.x)
  ),
  
  # get study park polygons from osm
  tar_target(
    offic_study_parks,
    join_study_parks(study_parks_file, city_parks)
  ),
  
  # full park spatial dataset
  tar_target(
    full_study_parks, 
    join_all_bounds(ind_bound, ag_bound, for_bound, mix_bound, offic_study_parks, study_parks_file)
  ),
  
  # save
  tar_target(
    save_full_study_parks,
    write_sf(full_study_parks, 'output/full_study_parks.gpkg')
  )
  
  
)
