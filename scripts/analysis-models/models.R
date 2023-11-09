targets_models <- c(
  
  tar_target(
    generative_data,
    data_generative()
  ),
  
  tar_target(
    real_data_cooling,
    data_model(temp_indices, 'cooling')
  ),
  
  tar_target(
    real_data_landuse,
    data_model(temp_indices, 'landuse')
  ),

# Assess priors -----------------------------------------------------------

  # Model 1: direct effect of tree variables on cooling and how those effects vary by past land use type 
  tar_target(
    prior_model_1,
    brm(mean_day ~ 1 +  Dens_L_s + DBH_med_L_s + SR_L_s + (1 | PastLandUse) + (1 | Park),
        data = real_data_cooling,
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
  
  # Model 2-4: total effect of past land use type on tree variables 
  tar_target(
    prior_model_2,
    brm(Dens_L ~ 1 + (1 | PastLandUse),
        data = real_data_landuse,
        family = gaussian(),
        prior = c(
          prior(normal(0, 0.5), class = "Intercept"),
          prior(exponential(1), class = "sigma")
        ),
        sample_prior = 'only',
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  ),

  tar_target(
    prior_model_3,
    brm(DBH_med_L ~ 1 + (1 | PastLandUse),
        data = real_data_landuse,
        family = gaussian(),
        prior = c(
          prior(normal(0, 0.5), class = "Intercept"),
          prior(exponential(1), class = "sigma")
        ),
        sample_prior = 'only',
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  ),

  tar_target(
    prior_model_4,
    brm(SR_L ~ 1 + (1 | PastLandUse),
        data = real_data_landuse,
        family = gaussian(),
        prior = c(
          prior(normal(0, 0.5), class = "Intercept"),
          prior(exponential(1), class = "sigma")
        ),
        sample_prior = 'only',
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  ),
  
  # Model 5: total effect of past land-use type on cooling
  tar_target(
    prior_model,
    brm(mean_day ~ 1 + (1 | PastLandUse) + (1 | Park),
        data = real_data_cooling,
        family = gaussian(),
        prior = c(
          prior(normal(0, 0.5), class = "Intercept"),
          prior(exponential(1), class = "sigma")
        ),
        sample_prior = 'only',
        backend = 'cmdstanr',
        iter = 1000,
        chains = 8)
  )

# Assess models -----------------------------------------------------------

  # Test model to see if it is returning what we expect w generative data 
  #tar_target(
  #  gen_model,
  #  brm(temp ~ 1 + (1 + dens_s | group) + (1 + size_s | group) + (1 + richness_s | group),
  #      data = generative_data,
  #      family = gaussian(),
  #      prior = c(
  #        prior(normal(0, 0.5), class = "Intercept"),
  #        prior(normal(0, 0.5), class = "b"),
  #        prior(exponential(1), class = "sigma")
  #      ),
  #      backend = 'cmdstanr',
  #      iter = 1000,
  #      chains = 8)
  #),
  
  #tar_target(
  #  generative_diagnostics,
  #  model_diagnostics(gen_model, 'generative')
  #)
  

# Assess data -------------------------------------------------------------


)