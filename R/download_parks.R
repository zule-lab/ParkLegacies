download_parks <- function(bbox, mtl) {
  # download island boundary in bbox
  parks <- opq(bbox) %>%
    add_osm_feature(key = 'leisure', value = 'park') %>%
    osmdata_sf()
  # grab multipolygons (large parks)
  mpolys <- parks$osm_multipolygons
  mpolys <- st_make_valid(st_cast(mpolys, "POLYGON"))
  # grab polygons (small parks)
  polys <- parks$osm_polygons
  allpolys <- st_make_valid(st_union(polys, mpolys))
  # clip parks to Montreal island boundary
  wi <- st_within(allpolys, mtl)
  subwi <- vapply(wi, function(x) length(x) >= 1, TRUE)
  keepp <- allpolys[subwi, ]
  
  return(keepp)
  
}