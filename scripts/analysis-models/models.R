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
    model_prior,
    brm(temp ~ 1 + group:dens_s + group:size_s + group:richness_s,
        data = generative_data,
        family = gaussian(),
        prior = c(
          prior(normal(0, 1), class = "Intercept"),
          prior(normal(0, 1), class = "b"),
          prior(exponential(2), class = "sigma")
        ),
        sample_prior = 'only',
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  ),
  
  tar_target(
    gen_model,
    brm(temp ~ 1 + group:dens_s + group:size_s + group:richness_s,
        data = generative_data,
        family = gaussian(),
        prior = c(
          prior(normal(0, 1), class = "Intercept"),
          prior(normal(0, 1), class = "b"),
          prior(exponential(2), class = "sigma")
        ),
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  )
  
)