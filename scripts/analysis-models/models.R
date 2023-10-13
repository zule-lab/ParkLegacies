targets_models <- c(
  
  tar_target(
    generative_data,
    data_generative()
  ),
  
  tar_target(
    model_data,
    data_model(temp_indices)
  )
  
)