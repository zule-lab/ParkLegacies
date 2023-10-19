targets_models <- c(
  
  tar_target(
    generative_data,
    data_generative()
  ),
  
  tar_target(
    real_data,
    data_model(temp_indices)
  ),
  
  tar_target(
    gen_model,
    brm(temp ~ group + dens_s + size_s + richness_s,
        data = generative_data,
        family = gaussian(),
        prior = c(
          prior(normal(0, 0.5), class = "Intercept"),
          prior(normal(0, 0.5), class = "b"),
          prior(exponential(2), class = "sigma")
        ),
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  )
  
)