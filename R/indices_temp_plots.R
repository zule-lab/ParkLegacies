indices_temp_plots <- function(temp_clean, sensor_pts, sensor_con_pts, trees_plots){
  
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
    group_by(plot_id, Park, date, tod) %>%
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
    left_join(., trees_plots, by = join_by(plot_id == PlotID))
  
  cooling <- temp_diff %>% 
    rename(Park = Park.x) %>% 
    select(-Park.y) %>%
    rowwise() %>% 
    mutate(cooling = mean_con - mean)
  
}