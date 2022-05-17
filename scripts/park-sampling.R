park_sampling <- c(
  
  tar_target(
    cc_file,
    'input/660_IndiceCanopee_2019.tif',
    format = "file"
  ),
  
  tar_target(
    cc,
    read_stars(cc_file)
  ),
  
  tar_target(
    parks_trans, 
    st_transform(full_study_parks, st_crs(cc))
  ),
  
  # make 0.04 ha sampling plots in each park
  tar_target(
    sample_grid, 
    st_intersection(parks_trans, st_make_grid(parks_trans, cellsize = 20))
  ),
  
  # calculate percent canopy cover based on number of cells == canopy / number of cells total
  tar_target(
    cc_gr,
    aggregate(cc, sample_grid, FUN = function(x) sum(x == 4)/length(x)) %>% 
      st_as_sf() %>% 
      rename(percan = `660_IndiceCanopee_2019.tif`)
  ),
  
  # create categorical variable for canopy cover
  tar_target(
    cc_cat, 
    cc_gr %>% 
      mutate(catcan = case_when(
        percan >= 0.10 & percan <= 0.30 ~ 'low',
        percan >= 0.31 & percan <= 0.80  ~ 'med',
        percan >= 0.81 & percan <= 1.00  ~ 'high'))
  ),
  
  # intersect aggregated canopy measurements with associated parks 
  tar_target(
    cc_parks, 
    st_intersection(cc_cat, parks_trans)
  ),
  
  # randomly sample three plots per level per park 
  tar_target(
    sp_pts,
    pts_sp(cc_parks)
  ),
  
  tar_target(
    save_sp_pts, 
    st_write(sp_pts, "output/park_sampling_points.kml")
  )
  
)
