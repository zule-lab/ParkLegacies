indices_temp <- function(temp_clean, sensor_pts, sensor_con_pts){
  
 con_tod <- calc_tod(temp_clean, sensor_con_pts)

 parks_tod <- calc_tod(temp_clean, sensor_pts)
 
 
  
  
}


calc_tod <- function(temp_clean, pts){
  
  pts_zm <- st_zm(pts)
  
  pts_d = pts_zm[!st_is_empty(pts_zm),,drop=FALSE] 
  
  pts_s <- pts_d %>%
    select(c(name, geometry))
  
  coords <- temp_clean %>%
    inner_join(pts_s, by = c("plot_id" = "name")) %>%
    mutate(lon = st_coordinates(geometry)[,1],
           lat = st_coordinates(geometry)[,2],
           date = as.Date(date_time, tz = 'America/Toronto'))
  
  sun <- coords %>% 
    select(c(date, lat, lon)) %>%
    getSunlightTimes(data = ., tz = 'America/Toronto', keep = c('sunrise', 'sunset')) %>%
    select(c(sunrise, sunset)) %>%
    cbind(coords) %>%
    mutate(tod = case_when(date_time >= sunrise & date_time <= sunset ~ 'day',
                           date_time < sunrise | date_time > sunset ~ 'night'))
  
  return(sun)
  
  
}
