#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

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
