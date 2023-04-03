target_park_prep <- c(
  
  # get file path for study park csv 
  tar_target(
    study_parks_file,
    'input/studyparks.csv',
    format = "file"
    ),
  
  # download municipal park polygons
  tar_target(
    city_parks, 
    download_shp("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/download/shapefile.zip",
                 "input/mon_park_raw.zip")
    ),
  
  # TODO: fix west island parks
  tar_target(
    offic_study_parks,
    join_study_parks(study_parks_file, city_parks)
    ),
  
  tar_target(
    save_parks,
    write_sf(offic_study_parks, "output/offic_parks_spatial.gpkg")
    )
  
)
