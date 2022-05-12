outline_mtl <- function(bbox) {
  # download island boundary in bbox
  mtl <- opq(bbox) %>%
    add_osm_feature(key = 'place', value = 'island') %>%
    osmdata_sf()
  # grab multipolygons (large islands)
  multipolys <- mtl$osm_multipolygons
  # grab polygons (small islands)
  polys <- mtl$osm_polygons
  polys <- st_cast(polys, "MULTIPOLYGON")
  # combine geometries and cast as sf
  allpolys <- st_as_sf(st_union(polys, multipolys))
  
  return(allpolys)
}