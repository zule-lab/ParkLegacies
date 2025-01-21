target_models <- c(

# Modelling -----------------------------------------------------------

  # Model 1: direct effect of tree variables on cooling and how those effects vary by past land use type 
  zar_brms(
    model_1,
    formula = cooling_s ~ 1 + BasalArea_L_s + DBH_med_L_s + SR_L_s +  BasalArea_S_s + DBH_med_S_s + (1 | tod) + (1 | Park),
    family = gaussian(),
    prior = c( 
          prior(normal(0, 0.5), class = "b"),
          prior(normal(0, 0.5), class = "Intercept"),
          prior(normal(0, 0.2), class = "sd"),
          prior(exponential(1), class = "sigma")
        ),
    backend = 'cmdstanr',
    data = temp_indices %>% 
      ungroup() %>% 
      mutate(Park = as.factor(Park),
             PastLandUse = as.factor(PastLandUse),
             tod = as.factor(tod),
             cooling_s = scale(cooling),
             SR_L_s = scale(SR_L), 
             DBH_med_L_s = scale(DBH_med_L), 
             Dens_L_s = scale(Dens_L),
             Dens_S_s = scale(Dens_S),
             DBH_med_S_s = scale(DBH_med_S),
             BasalArea_S_s = scale(BasalArea_S), 
             BasalArea_L_s = scale(BasalArea_L)),
    chains = 4,
    #control = list(adapt_delta = 0.9),
    iter = 1000,
    cores = 4
  ),
  
  # Model 2-4: total effect of past land use type on tree variables 
  zar_brms(
    model_2_L,
    formula = BasalArea_L_s ~ 1 + PastLandUse,
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, BasalArea_L, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             BasalArea_L_s = scale(BasalArea_L)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(-1.5, 2), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_2_S,
    formula = BasalArea_S_s ~ 1 + PastLandUse,
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, BasalArea_S, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             BasalArea_S_s = scale(BasalArea_S)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(-1, 5), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_3_L,
    formula = DBH_med_L_s ~ 1 + PastLandUse,
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, DBH_med_L, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             DBH_med_L_s = scale(DBH_med_L)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(2.5, 1), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_3_S,
    formula = DBH_med_S_s ~ 1 +  PastLandUse,
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, DBH_med_S, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             DBH_med_S_s = scale (DBH_med_S)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(1, 0.5), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_4_L,
    formula = SR_L_s ~ 1 +  PastLandUse,
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, SR_L, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             SR_L_s = scale(SR_L)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(2, 1), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  # Model 5: total effect of past land-use type on cooling
  zar_brms(
    model_5,
    formula = cooling_s ~ 1 + PastLandUse + Age_s + (1 | tod) + (1 | Park),
    data = temp_indices %>% 
      ungroup() %>% 
      mutate(Park = as.factor(Park),
             PastLandUse = as.factor(PastLandUse),
             tod = as.factor(tod),
             cooling_s = scale(cooling),
             Age_s = scale(Age)),
    family = gaussian(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  tar_target(
    model_list,
    list(model_1_brms_sample, model_2_L_brms_sample, model_2_S_brms_sample, model_3_L_brms_sample, 
         model_3_S_brms_sample, model_4_L_brms_sample, model_5_brms_sample) %>%
      setNames(., c('model_1', 'model_2_L', 'model_2_S', 'model_3_L', 'model_3_S', 'model_4_L', 'model_5'))
  ),

  tar_target(
    prior_model_list,
    list(model_1_brms_sample_prior, model_2_L_brms_sample_prior, model_2_S_brms_sample_prior, model_3_L_brms_sample_prior, 
         model_3_S_brms_sample_prior, model_4_L_brms_sample_prior, model_5_brms_sample_prior) %>%
      setNames(., c('model_1_prior', 'model_2_L_prior', 'model_2_S_prior', 'model_3_L_prior', 'model_3_S_prior', 'model_4_L_prior', 'model_5_prior'))
    
  ),
  
  # prior checks 
  tar_render(
    prior_predictive,
    'graphics/diagnostics/prior_predictive.qmd'
  ),

  # model diagnostics
  tar_render(
    model_diagnostics,
    'graphics/diagnostics/model_diagnostics.qmd'
  ),

  tar_target(
    socioec_data,
    data_socioec(census)
  )
  
  
)