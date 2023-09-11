download_csv <- function(url, dest){
  
  temp <- tempfile()
    
  download.file(url, dest, mode = "wb")
  
  master <- as.character(unzip(dest, list = TRUE)$Name)
  
  df <- read.csv(unz("input/quebec_census_2021/census.zip", master[1]))

  return(df)
}