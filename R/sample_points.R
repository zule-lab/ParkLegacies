sample_points <- function(cc_file, full_study_parks) {

    cc <- read_stars(cc_file)
  
 
    parks_trans <- st_transform(full_study_parks, st_crs(cc))
  
  # make 0.04 ha sampling plots in each park
    sample_grid <- st_intersection(parks_trans, st_make_grid(parks_trans, cellsize = 20))
  
  # calculate percent canopy cover based on number of cells == canopy / number of cells total
    cc_gr <- aggregate(cc, sample_grid, FUN = function(x) sum(x == 4)/length(x)) %>% 
      st_as_sf() %>% 
      rename(percan = `660_IndiceCanopee_2019.tif`)
  
    wat_gr <- aggregate(cc, sample_grid, FUN = function(x) sum(x == 5)/length(x)) %>%
      st_as_sf() %>%
      rename(perwat = `660_IndiceCanopee_2019.tif`)
  
    wat_cc_j <- st_join(cc_gr, wat_gr, join = st_equals_exact, par = 1e-9)

  # create categorical variable for canopy cover
    cc_cat <- wat_cc_j %>% 
      mutate(catcan = case_when(
        percan >= 0.10 & percan <= 0.30 & perwat == 0 ~ 'low',
        percan >= 0.31 & percan <= 0.80 & perwat == 0 ~ 'med',
        percan >= 0.81 & percan <= 1.00 & perwat == 0 ~ 'high'))
  
  # intersect aggregated canopy measurements with associated parks 
    cc_parks <- st_intersection(cc_cat, parks_trans)
  
  # randomly sample three plots per level per park 
    sp_pts <- pts_sp(cc_parks)
    
    return(sp_pts)
  
  
}



pts_sp <- function(cc_parks) {
  
  s <- cc_parks %>% 
    drop_na %>%
    group_by(Name, PastLandUse, catcan) %>% 
    sample_n(size = 5, replace = T) %>% 
    distinct(.keep_all = TRUE) %>%
    ungroup()
  
  sp_pts <- st_centroid(s)
  
  sp_pts_t <- st_transform(sp_pts, 4326)
  
  sp_pts_f <- sp_pts_t %>% 
    group_split(PastLandUse) %>% 
    purrr::map_df(~.x %>% group_by(Name, .add = T) %>% mutate(ParkID = cur_group_id())) %>%
    group_by(Name, PastLandUse, catcan) %>%
    mutate(PlotID = paste0(str_to_upper(substr(PastLandUse, 1, 3)), ParkID, "-", str_to_upper(catcan), 0, row_number()))
  
  return(sp_pts_f)
  
}
