indices_temp <- function(temp_clean, sensor_pts, sensor_con_pts, trees_indices){

  # calculate time of day for all temp pts
  con_tod <- calc_tod(temp_clean, sensor_con_pts)
  parks_tod <- calc_tod(temp_clean, sensor_pts)
  
  # calculate max and mean temps for day and night 
  con_calc <- con_tod %>%
    group_by(plot_id, date, tod) %>%
    summarize(max = max(temperature_c),
              mean = mean(temperature_c)) %>%
    rename(con_id = plot_id)
 
  parks_calc <- parks_tod %>%
    group_by(Park, date, tod) %>%
    summarize(max = max(temperature_c),
              mean = mean(temperature_c))
  
  # calculate difference between parks and controls 
  temp_diff <- parks_calc %>%
    mutate(con_id = case_when(Park == 'Adrien-D.-Archambault' ~ 'CON-AT', # this control was stolen so going w closest one
                              Park == 'Angrignon' ~ 'CON-ANG',
                              Park == 'Arthur-Therrien' ~ 'CON-AT',
                              Park == 'Baldwin' ~ 'CON-LAL-LAF-JAR',
                              Park == 'Bois-de-Liesse' ~ 'CON-BDL',
                              Park == 'Bois-de-Saraguay' ~ 'CON-BDS',
                              Park == 'Boisé-du-Saint-Sulpice' ~ 'CON-FB-SS',
                              Park == 'Cap Saint-Jacques' ~ 'CON-LALO-CSJ',
                              Park == 'Coulée Grou' ~ 'CON-PaP-CG',
                              Park == 'Félix-Leclerc' ~ 'CON-F-L',
                              Park == 'Frédéric-Back' ~ 'CON-FB-SS',
                              Park == 'Ile-Bizard' ~ 'CON-IB',
                              Park == 'Jardin Botanique' ~ 'CON-LAL-LAF-JAR',
                              Park == 'Jean-Drapeau' ~ 'CON-LAL-LAF-JAR',
                              Park == "L'Anse-À-L'Orme" ~ 'CON-LALO-CSJ',
                              Park == 'La Fontaine' ~ 'CON-LAL-LAF-JAR',
                              Park == 'Lafond' ~ 'CON-LAL-LAF-JAR',
                              Park == 'Lalancette' ~ 'CON-LAL-LAF-JAR',
                              Park == 'Marguerite-Bourgeoys' ~ 'CON-MB-SG',
                              Park == 'Parc Centennial' ~ 'CON-CENTENNIAL', 
                              Park == 'Parc Fritz' ~ 'CON-Fritz', 
                              Park == 'Parc Saint-Gabriel' ~ 'CON-MB-SG',
                              Park == 'Père-Marquette ' ~ 'CON-P-M',
                              Park == 'Pointe-aux-Prairies' ~ 'CON-PaP-CG',
                              Park == 'Promenade-Bellerive' ~ 'CON-P-B',
                              Park == 'Thomas-Chapais' ~ 'CON-P-B'
                              )) %>%
    left_join(., con_calc, by = c('con_id', 'date', 'tod'), suffix = c("", "_con")) %>%
    left_join(., trees_indices, by = 'Park')
  
  cooling <- temp_diff %>% 
    rowwise() %>% 
    mutate(cooling = mean_con - mean)

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
