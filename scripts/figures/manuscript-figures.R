target_manuscript_figures <- c(
  
  tar_target(
    figure_1, 
    create_figure_1()
  ),
  
  tar_target(
    figure_2, 
    create_figure_2(full_study_parks, sp_pts)
  ),
  
  tar_target(
    plu_temp_total,
    create_plu_temp_total(model_list, temp_indices)
  ),
  
  tar_target(
    plu_tree_direct,
    create_plu_tree_direct(model_list)
  ),
  
  tar_target(
    tree_temp_direct,
    create_tree_temp_direct(model_list, temp_indices)
  ),
  
  tar_render(
    tree_temp_direct_jitter,
    'graphics/tree_temp_direct_jitter.qmd'
  ),
  
  tar_render(
    model_equations,
    'scripts/figures/equations.qmd'
  )
  
  
  
)