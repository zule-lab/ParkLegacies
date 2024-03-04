target_census_prep <- c(
  
  # download DA shapefiles
  tar_target(
    da_raw, 
    download_shp("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip",
                 "input/da_raw/")
  ),
  
  # download census data
  tar_target(
    census_raw,
    download_csv("https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&FILETYPE=CSV&GEONO=006_Quebec")
  ),
  
  tar_target(
    census,
    clean_census_da(full_study_parks, da_raw, census_raw)
  )
  
)
