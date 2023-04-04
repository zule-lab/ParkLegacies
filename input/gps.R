
# Import & Save GPS Points ------------------------------------------------


library(gpx)
library(sf)
library(dplyr)

f <- function(x) {
  gpx_file <- read_gpx(x)
  pts <- gpx_file$waypoints
  pts_sf <- st_as_sf(pts, coords = c("Longitude", "Latitude"), crs = 4326)
  
  return(pts_sf)
}

setwd("H:/Garmin/GPX")

pts <- lapply(grep('Waypoints_', list.files(file.path("H:/Garmin/GPX/")), value = TRUE),  f)

pts_df <- Reduce(rbind, pts)

setwd("C:/Users/I_RICHMO/Documents/Repositories/park-legacies")

write_sf(pts_df, "input/gps.gpkg")

