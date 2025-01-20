target_tree_prep <- c(
  
  tar_file_read(
    trees_raw,
    'input/trees_openrefine.csv',
    read.csv(!!.x)
    
  ),
  
  tar_target(
    trees_clean,
    clean_trees(trees_raw, field_cc)
  ),
  
  tar_target(
    trees_indices,
    indices_trees(trees_clean, full_study_parks)
  ),
  
  tar_target(
    trees_plots,
    indices_plots_trees(trees_clean, field_sp_pts, full_study_parks)
  )
  

  
)
