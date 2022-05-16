#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

join_all_bounds <- function(handmade, official) {

  missing <- anti_join(official, as.data.frame(handmade), by = "Name")
  
  missing <- missing %>% 
    summarise(Name = Name,
           PastLandUse = PastLandUse, 
           geom = geometry)
  
  all <- rbind(handmade, missing)
  
  return(all)
  


}
