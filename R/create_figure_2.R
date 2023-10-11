create_figure_2 <- function(full_study_parks, sp_pts){
  

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
  
  ggplot() +
    geom_sf(fill = '#ceb99b', data = allpolys) + 
    geom_sf(fill = '#678d58', col = NA, data = full_study_parks) +
    geom_sf(fill = '#99acc3', data = mpols) + 
    coord_sf(xlim = c(bbi['xmin'], bbi['xmax']),
             ylim = c(bbi['ymin'], bbi['ymax'])) +
    theme(panel.border = element_rect(size = 1, fill = NA),
          panel.background = element_rect(fill = '#ddc48d'),
          panel.grid = element_line(color = '#73776F', size = 0.2),
          axis.text = element_text(size = 11, color = 'black'),
          axis.title = element_blank(), 
          plot.background = element_rect(fill = NA, colour = NA))

# Single park plot --------------------------------------------------------
  
  
  

# Sampling plot -----------------------------------------------------------
  
  
  

# All ---------------------------------------------------------------------

  
  
}