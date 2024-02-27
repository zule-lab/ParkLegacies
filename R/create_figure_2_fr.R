create_figure_2_fr <- function(full_study_parks, field_sp_pts){
  
  
  # Montreal boundary files -------------------------------------------------
  bb <- c(xmin = -74.0788,
          ymin = 45.3414,
          xmax = -73.3894,
          ymax = 45.7224)
  
  ## Montreal
  # Download island boundary in bbox
  mtl <- opq(bb) %>%
    add_osm_feature(key = 'place', value = 'island') %>%
    osmdata_sf()
  # Grab multipolygons (large islands)
  multipolys <- mtl$osm_multipolygons
  # Grab polygons (small islands)
  polys <- mtl$osm_polygons
  polys <- st_cast(polys, "MULTIPOLYGON")
  # Combine geometries and cast as sf
  allpolys <- st_as_sf(st_union(polys, multipolys))
  
  ## Water
  water <- opq(bb) %>%
    add_osm_feature(key = 'natural', value = 'water') %>%
    osmdata_sf()
  mpols <- water$osm_multipolygons
  mpols <- st_cast(mpols, "MULTIPOLYGON")
  mpols <- st_as_sf(st_make_valid(mpols))
  
  
  # Montreal plot -----------------------------------------------------------
  
  bbi <- st_bbox(st_buffer(allpolys, 2.5))
  
  # transform study parks into points for easier visualization
  study_pts <- full_study_parks %>% 
    filter(Name != 'Boisé Jean-Milot' & Name != 'Monseigneur-J.-A.-Richard') %>%
    st_make_valid() %>%
    st_centroid()
  
  study_pts$PastLandUse <- factor(study_pts$PastLandUse, levels = c('Industrial', 'Agricultural', 'Forested'))
  
  mtl <- ggplot() +
    geom_sf(fill = '#ceb99b', data = allpolys) + 
    geom_sf(fill = '#99acc3', data = mpols) + 
    geom_sf(aes(colour = PastLandUse), size = 3, data = study_pts) +
    coord_sf(xlim = c(bbi['xmin'], bbi['xmax']),
             ylim = c(bbi['ymin'], bbi['ymax'])) +
    scale_colour_manual(values = c("#c2d6a4", "#669d62", "#1e3d14"), labels = c('Industriel', 'Agricole', 'Forestier')) + 
    labs(colour = "") + 
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#ddc48d'),
          panel.grid = element_line(color = '#73776F', linewidth = 0.2),
          axis.text = element_text(size = 16, color = 'black'),
          legend.text = element_text(size = 16), 
          axis.title = element_blank(), 
          plot.background = element_rect(fill = NA, colour = NA),
          legend.position = 'top')
  
  
  # Single park plot --------------------------------------------------------
  
  ang <- full_study_parks %>%
    filter(Name == 'Angrignon')
  
  ang_pts <- field_sp_pts %>%
    filter(str_detect(PlotID, 'FOR1-|AGR1-')) %>% 
    mutate(canopy = case_when(str_detect(PlotID, 'HIGH') ~ 'Élevé',
                              str_detect(PlotID, 'MED') ~ 'Moyen',
                              str_detect(PlotID, 'LOW') ~ 'Bas')) 
  ang_pts$canopy <- factor(ang_pts$canopy, levels = c('Bas', 'Moyen', 'Élevé'))
  
  bba <- st_bbox(ang)
  
  angrignon <- ggplot() + 
    geom_sf(data = ang, aes(fill = PastLandUse)) + 
    geom_sf(fill = '#99acc3', data = mpols) +
    geom_sf(data = ang_pts, aes(shape = canopy), color = "black", size = 3.5) + 
    coord_sf(xlim = c(bba['xmin'], bba['xmax']),
             ylim = c(bba['ymin'], bba['ymax'])) +
    scale_fill_manual(values = c("#669d62", "#1e3d14"), labels = c('Agricole', 'Forestier')) +
    scale_shape_manual(values = c(2, 4, 16)) + 
    labs(fill = "", shape = "") +
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#ddc48d'),
          panel.grid = element_line(color = '#73776F', linewidth = 0.2),
          axis.text = element_text(size = 16, color = 'black'),
          legend.text = element_text(size = 16), 
          axis.title = element_blank(), 
          plot.background = element_rect(fill = NA, colour = NA),
          legend.position = 'top')
  
  
  
  
  # All ---------------------------------------------------------------------
  
  arrowA <- data.frame(x1 = 14.75, x2 = 23, y1 = 6, y2 = 8.75)
  
  full <- ggplot() +
    coord_equal(xlim = c(0, 40), ylim = c(0, 20), expand = FALSE) +
    annotation_custom(ggplotGrob(mtl), xmin = 0, xmax = 20, ymin = 0, 
                      ymax = 20) +
    annotation_custom(ggplotGrob(angrignon), xmin = 20, xmax = 40, ymin = 3, 
                      ymax = 18) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
                 linewidth = 1, arrow = arrow(), lineend = "round") +
    
    theme_void() 
  
  ggsave('graphics/figure_2_fr.png', full, width = 12, height = 14, units = 'in', dpi = 450)
  
}



