target_models_plots <- c(
  
  # Modelling -----------------------------------------------------------
  
  # Model 1: direct effect of tree variables on cooling and how those effects vary by past land use type 
  zar_brms(
    tree_temp,
    formula = cooling_s ~ 1 + BasalArea_L_s + DBH_med_L_s + SR_L_s +  BasalArea_S_s + DBH_med_S_s + 
      + BasalArea_L_s:tod + DBH_med_L_s:tod + SR_L_s:tod + BasalArea_S_s:tod + DBH_med_S_s:tod + (1 | Park) + (1 | plot_id),
    family = gaussian(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    data = temp_plots %>% 
      ungroup() %>% 
      mutate(plot_id = as.factor(plot_id),
             Park = as.factor(Park),
             PastLandUse = as.factor(PastLandUse),
             tod = as.factor(tod),
             cooling_s = scale(cooling),
             SR_L_s = scale(SR_L), 
             DBH_med_L_s = scale(DBH_med_L), 
             DBH_med_S_s = scale(DBH_med_S),
             BasalArea_S_s = scale(BasalArea_S), 
             BasalArea_L_s = scale(BasalArea_L)),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  # Model 2-4: total effect of past land use type on tree variables 
  zar_brms(
    plu_density_L,
    formula = BasalArea_L_s ~ 1 + PastLandUse,
    data = temp_plots %>% 
      ungroup() %>%
      select(c(plot_id, BasalArea_L, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             BasalArea_L_s = scale(BasalArea_L)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),
  
  zar_brms(
    plu_density_S,
    formula = log(BasalArea_S_s) ~ 1 + PastLandUse,
    data = temp_plots %>% 
      ungroup() %>%
      select(c(plot_id, BasalArea_S, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             BasalArea_S_s = scale(BasalArea_S)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),
  
  zar_brms(
    plu_size_L,
    formula = DBH_med_L_s ~ 1 + PastLandUse,
    data = temp_plots %>% 
      ungroup() %>%
      select(c(plot_id, DBH_med_L, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             DBH_med_L_s = scale(DBH_med_L)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(-1, 0.5), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),
  
  zar_brms(
    plu_size_S,
    formula = DBH_med_S_s ~ 1 +  PastLandUse,
    data = temp_plots %>% 
      ungroup() %>%
      select(c(plot_id, DBH_med_S, PastLandUse)) %>% 
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
    plu_SR_L,
    formula = SR_L_s ~ 1 +  PastLandUse,
    data = temp_plots %>% 
      ungroup() %>%
      select(c(plot_id, SR_L, PastLandUse)) %>% 
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
    plu_temp,
    formula = cooling_s ~ 1 + PastLandUse + tod + PastLandUse:tod + Age_s + (1 | Park) + (1 | plot_id),
    data = temp_plots %>% 
      ungroup() %>% 
      mutate(plot_id = as.factor(plot_id),
             Park = as.factor(Park),
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
    model_list_plots,
    list(tree_temp_brms_sample, plu_density_L_brms_sample, plu_density_S_brms_sample, plu_size_L_brms_sample, 
         plu_size_S_brms_sample, plu_SR_L_brms_sample, plu_temp_brms_sample) %>%
      setNames(., c('tree_temp', 'plu_density_L', 'plu_density_S', 'plu_size_L', 'plu_size_S', 'plu_SR_L', 'plu_temp'))
  ),
  
  tar_target(
    prior_model_list_plots,
    list(tree_temp_brms_sample_prior, plu_density_L_brms_sample_prior, plu_density_S_brms_sample_prior, plu_size_L_brms_sample_prior, 
         plu_size_S_brms_sample_prior, plu_SR_L_brms_sample_prior, plu_temp_brms_sample_prior) %>%
      setNames(., c('tree_temp_prior', 'plu_density_L_prior', 'plu_density_S_prior', 'plu_size_L_prior', 'plu_size_S_prior', 'plu_SR_L_prior', 'plu_temp_prior'))
    
  ),
  
  # prior checks 
  tar_render(
    prior_predictive_plots,
    'graphics/diagnostics/prior_predictive_plots.qmd'
  ),
  
  # model diagnostics
  tar_render(
    model_diagnostics_plots,
    'graphics/diagnostics/model_diagnostics_plots.qmd'
  )
  
)