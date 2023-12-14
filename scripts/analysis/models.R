targets_models <- c(

# Modelling -----------------------------------------------------------

  # Model 1: direct effect of tree variables on cooling and how those effects vary by past land use type 
  zar_brms(
    model_1,
    formula = cooling_s ~ 1 + Dens_L_s + DBH_med_L_s + SR_L_s +  Dens_S_s + DBH_med_S_s + (1 | tod) + (1 | Park),
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
             DBH_med_S_s = scale(DBH_med_S)),
    chains = 4,
    #control = list(adapt_delta = 0.9),
    iter = 1000,
    cores = 4
  ),
  
  # Model 2-4: total effect of past land use type on tree variables 
  zar_brms(
    model_2_L,
    formula = Dens_L_log ~ 1 + (1| PastLandUse),
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, Dens_L, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             Dens_L_log = log(Dens_L)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(7, 2), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_2_S,
    formula = Dens_S_log ~ 1 + (1 | PastLandUse),
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, Dens_S, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             Dens_S_log = log(Dens_S)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(8, 3), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_3_L,
    formula = DBH_med_L_log ~ 1 + (1 | PastLandUse),
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, DBH_med_L, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             DBH_med_L_log = log(DBH_med_L)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(2.5, 1), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_3_S,
    formula = DBH_med_S_log ~ 1 + (1 | PastLandUse),
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, DBH_med_S, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             DBH_med_S_log = log(DBH_med_S)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(1, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_4_L,
    formula = SR_L_log ~ 1 + (1 | PastLandUse),
    data = temp_indices %>% 
      ungroup() %>%
      select(c(Park, SR_L, PastLandUse)) %>% 
      mutate(PastLandUse = as.factor(PastLandUse),
             SR_L_log = log(SR_L)) %>% 
      distinct(),
    family = gaussian(),
    prior = c(
      prior(normal(2, 1), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
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
    list(model_1_brms_sample_prior, model_5_brms_sample_prior) %>%
      setNames(., c('prior_model_1', 'prior_model_5'))
  ),
  
  # add prior checks 
  tar_target(
    model_prior_plots,
    plot_prior(prior_model_list, -3, 3, names(prior_model_list)),
    pattern = map(prior_model_list),
    iteration = "list"
  ),

  # model diagnostics
  tar_render(
    model_diagnostics,
    'graphics/diagnostics/model_diagnostics.qmd'
  )
  
  
)