#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mix_bound
parks_mix <- function(mix_bound) {
  
  m <- st_make_valid(mix_bound)

  u <- m %>% 
    dplyr::group_by(Name, PastLandUse) %>%
    dplyr::summarise(PastLandUse = dplyr::first(PastLandUse),
                     geom = st_union(geom))
}
