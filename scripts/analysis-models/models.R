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
    prior_model,
    brm(temp ~ 1 + group:dens_s + group:size_s + group:richness_s,
        data = generative_data,
        family = gaussian(),
        prior = c(
          prior(normal(0, 0.5), class = "Intercept"),
          prior(normal(0, 0.5), class = "b"),
          prior(exponential(1), class = "sigma")
        ),
        sample_prior = 'only',
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  ),
  
  tar_target(
    prior_diagnostics,
    model_diagnostics(prior_model, 'prior_only')
  ),
  
  tar_target(
    prior_range,
    conditional_effects(prior_model)
    # prior ranges look representative of data ranges
  ),
  
  tar_target(
    gen_model,
    brm(temp ~ 1 + group:dens_s + group:size_s + group:richness_s,
        data = generative_data,
        family = gaussian(),
        prior = c(
          prior(normal(0, 0.5), class = "Intercept"),
          prior(normal(0, 0.5), class = "b"),
          prior(exponential(1), class = "sigma")
        ),
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  ),
  
  tar_target(
    generative_diagnostics,
    model_diagnostics(gen_model, 'generative')
  ),
  
  tar_target(
    max_day_model,
    brm(max_day ~ 1 + PastLandUse:Dens_L_s +  PastLandUse:SR_L_s +  PastLandUse:DBH_med_L_s,
        data = real_data,
        family = gaussian(),
        prior = c(
          prior(normal(0, 0.5), class = "Intercept"),
          prior(normal(0, 0.5), class = "b"),
          prior(exponential(1), class = "sigma")
        ),
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  )
  
)