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
        distinct(.keep_all = TRUE)
  
  sp_pts <- st_centroid(s)
  
  sp_pts_t <- st_transform(sp_pts, 4326)
  
  return(sp_pts_t)

}
