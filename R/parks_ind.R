#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ind_parks_raw
parks_ind <- function(ind_parks_raw) {

  u <- ind_parks_raw %>% 
    dplyr::filter(Localisati != "Localisation indéterminée - élément ponctuel approximatf") %>%
    dplyr::group_by(Name) %>%
    dplyr::summarise(geometry = st_union(geometry),
                     PastLandUse = dplyr::first(PastLandUse),
                     Type = dplyr::first(Type),
                     Id_carrier = list(Id_carrier))
  
  
}
