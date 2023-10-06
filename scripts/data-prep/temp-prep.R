target_temp_prep <- c(
  
  tar_files(
    temp_files,
    dir('input/temperature', full.names = TRUE)
  ),
  
  tar_target(
    temp_dfs, 
    read.csv(temp_files) %>%
      mutate(plot_id = str_extract(basename(xfun::sans_ext(temp_files)), "[^_]+")),
    pattern = map(temp_files)
  ),
  
  tar_target(
    temp_clean,
    clean_temp(temp_dfs)
  )
  
  
  
  
  
  
)
