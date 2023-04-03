worm_samples <- function(sp_pts_buff){
  
  # randomly sample three points within each buffer for earthworm quadrats
  worm_pts_geom <- st_sample(sp_pts_buff, size = c(3,3), type = "random", by_polygon = T) %>% 
    st_as_sf() %>%
    mutate(GroupID = rep(seq_len(nrow(.)/3), each=3))
  
  
  # create unique ids for earthworm quadrats
  worm_pts <- sp_pts_buff %>% 
    ungroup() %>% 
    mutate(GroupID = row_number()) %>% 
    st_drop_geometry() %>% 
    left_join(worm_pts_geom, ., by = "GroupID") %>% 
    group_by(PlotID) %>% 
    mutate(QuadratID = paste0(PlotID, "-", 0, row_number())) %>%
    select(-GroupID)
  
  return(worm_pts)
  
  
}