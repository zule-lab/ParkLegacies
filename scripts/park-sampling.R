target_park_sampling <- c(
  
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
  
  tar_target(
    wat_gr,
    aggregate(cc, sample_grid, FUN = function(x) sum(x == 5)/length(x)) %>%
      st_as_sf() %>%
      rename(perwat = `660_IndiceCanopee_2019.tif`)
  ),
  
  tar_target(
    wat_cc_j,
    st_join(cc_gr, wat_gr, join = st_equals_exact, par = 1e-9)
  ),
  
  # create categorical variable for canopy cover
  tar_target(
    cc_cat, 
    wat_cc_j %>% 
      mutate(catcan = case_when(
        percan >= 0.10 & percan <= 0.30 & perwat == 0 ~ 'low',
        percan >= 0.31 & percan <= 0.80 & perwat == 0 ~ 'med',
        percan >= 0.81 & percan <= 1.00 & perwat == 0 ~ 'high'))
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
  
  # create 20 m x 20 m buffers surrounding the points for department of Grands Parcs
  
  tar_target(
    sp_pts_buff,
    st_buffer(sp_pts, dist = 20)
  ),
  
  tar_target(
    save_sp_pts, 
    st_write(sp_pts, "output/park_sampling_points.kml", append = F)
  ),
  
  tar_target(
    save_buff, 
    st_write(sp_pts_buff, "output/park_sampling_plots.kml", append = F)
  )
  
)
