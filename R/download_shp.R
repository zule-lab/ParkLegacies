download_shp <- function(url, dest){
  
  temp <- tempfile()
  
  download.file(url, dest, mode = "wb")
  
  city_parks <- st_read(file.path("/vsizip", dest))
  
  return(city_parks)
  
}