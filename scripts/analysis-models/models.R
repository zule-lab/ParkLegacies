targets_models <- c(
  
  tar_target(
    model_data,
    data_model(temp_indices, trees_indices)
  ),
  
  tar_target(
    mean_day_model_prior,
    model_mean_day()
  )
  
)