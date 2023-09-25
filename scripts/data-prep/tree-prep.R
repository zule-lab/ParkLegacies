target_tree_prep <- c(
  
  tar_file_read(
    trees_raw,
    'input/trees_openrefine.csv',
    read.csv(!!.x)
    
  ),
  
  tar_target(
    trees_clean,
    clean_trees(trees_raw, field_cc)
  )
  

  
)
