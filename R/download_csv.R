download_csv <- function(url){
  
  temp <- tempfile()
    
  download.file(url, temp, mode = "wb")
  
  master <- as.character(unzip(temp, list = TRUE)$Name)
  
  df <- read.csv(unz(temp, master[1]))

  return(df)
}