#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ag_bound_file
bound_ag <- function(ag_bound_file) {
  
  # read in gpkg
  ag <- read_sf(ag_bound_file)
  
  # rename geometry to match other gpkg layers
  ag <- rename(ag, geom = geometry)
  
  return(ag)
  
  

}
