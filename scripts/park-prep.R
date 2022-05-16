park_prep <- c(
  
  # set bbox of Montreal
  tar_target(
    bb, 
    c(xmin = -74.0788,
      ymin = 45.3414,
      xmax = -73.3894,
      ymax = 45.7224)
    ),
  
  # get file path for study park csv 
  tar_target(
    study_parks_file,
    'input/studyparks.csv',
    format = "file"
  ),
  
  # read in study park csv 
  tar_target(raw_study_parks,
             fread(study_parks_file)),
  
  # get outline of island of Montreal
  tar_target(
    mtl_outline, 
    outline_mtl(bb)
    ),
  
  # download park polygons from osm
  tar_target(
    parks_download,
    download_parks(bb, mtl_outline)
  ),
  
  # download municipal park polygons
  tar_target(
    city_parks, 
    download_shp("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/download/shapefile.zip",
                        "input/mon_park_raw.zip")
  ),
  
  # transform municipal polygons to match osm
  tar_target(
    city_parks_trans, 
    st_transform(city_parks, 4326)
  ), 
  
  tar_target(
    full_study_parks,
    join_study_parks(raw_study_parks, city_parks_trans, parks_download)
  ),
  
  tar_target(
    save_parks,
    write_sf(full_study_parks, "output/parks_spatial.gpkg")
  )
  
)
