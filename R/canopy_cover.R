canopy_cover <- function(sp_pts, field_sp_pts, cancov) {
  
  # rename original dataset to indicate that it was the calcs previous to fieldwork
  sp_pts_old <- sp_pts %>%
    st_transform(., st_crs(cancov)) %>%
    select(-perwat) %>%
    rename(percan_2019 = percan,
           catcan_2019 = catcan) %>%
    st_set_geometry(NULL)
  
  
  # convert to same projection as canopy cover 
  sp_pts_trans <- st_transform(st_zm(field_sp_pts), st_crs(cancov))

  # create 20 m square buffers around sampling points representing plots 
  plots <- st_buffer(sp_pts_trans, dist = 20, nQuadSegs=1, endCapStyle = "SQUARE")
  
  # calculate canopy cover by dividing number of pixels == 4 (canopy) by total number of pixels within a buffer
  pts_can <- aggregate(cancov, plots, FUN = function(x) sum(x == 4)/length(x)) %>% 
    st_as_sf() %>% 
    rename(percan_2021 = `660_IndiceCanopee_2021.tif`) %>%
    mutate(percan_2021 = round(percan_2021, 2),
           PlotID = plots$PlotID,
           catcan_2021 = case_when(
             percan_2021 >= 0.00 & percan_2021 <= 0.30 ~ 'low',
             percan_2021 >= 0.31 & percan_2021 <= 0.80 ~ 'med',
             percan_2021 >= 0.81 & percan_2021 <= 1.00 ~ 'high'))
  
  # join the old data to the new data 
  full <- left_join(pts_can, sp_pts_old, by = "PlotID") %>%
    select(Name, PlotID, PastLandUse, percan_2019, percan_2021, catcan_2019, catcan_2021, geometry)
  
  
}