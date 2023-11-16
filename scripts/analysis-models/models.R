targets_models <- c(
  
  tar_target(
    real_data_cooling,
    data_model(temp_indices, 'cooling')
  ),
  
  tar_target(
    real_data_landuse,
    data_model(temp_indices, 'landuse')
  ),

# Modelling -----------------------------------------------------------

  # Model 1: direct effect of tree variables on cooling and how those effects vary by past land use type 
  zar_brms(
    model_1,
    formula = mean_day_s ~ 1 + mean_day_con_s + Dens_L_s + DBH_med_L_s + SR_L_s + (1 | Park),
    family = gaussian(),
    prior = c( 
          prior(normal(0, 0.5), class = "b"),
          prior(normal(0, 0.5), class = "Intercept"),
          prior(normal(0, 0.2), class = "sd"),
          prior(exponential(1), class = "sigma")
        ),
    backend = 'cmdstanr',
    data = real_data_cooling,
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  # Model 2-4: total effect of past land use type on tree variables 
  zar_brms(
    model_2,
    formula = Dens_L ~ 1 + (1 | PastLandUse),
    data = real_data_landuse,
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 2,
    cores = 2
  ),

  zar_brms(
    model_3,
    formula = DBH_med_L ~ 1 + (1 | PastLandUse),
    data = real_data_landuse,
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 2,
    cores = 2
  ),

  zar_brms(
    model_4,
    formula = SR_L ~ 1 + (1 | PastLandUse),
    data = real_data_landuse,
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 2,
    cores = 2
  ),
  
  # Model 5: total effect of past land-use type on cooling
  zar_brms(
    model_5,
    formula = mean_day_s ~ 1 + mean_day_con_s + PastLandUse + Age + (1 | Park),
    data = real_data_cooling,
    family = gaussian(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 2,
    cores = 2
  )

 # add model diagnostics
  
)