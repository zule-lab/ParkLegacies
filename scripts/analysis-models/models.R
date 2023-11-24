targets_models <- c(

# Modelling -----------------------------------------------------------

  # Model 1: direct effect of tree variables on cooling and how those effects vary by past land use type 
  zar_brms(
    model_1,
    formula = mean_s ~ 1 + mean_con_s + Dens_L_s + DBH_med_L_s + SR_L_s + (1 | tod) + (1 | Park),
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
             Age_s = scale(Age),
             mean_s = scale(mean), 
             mean_con_s = scale(mean_con),
             SR_L_s = scale(SR_L), 
             DBH_med_L_s = scale(DBH_med_L), 
             Dens_L_s = scale(Dens_L)),
    init = 0,
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  # Model 2-4: total effect of past land use type on tree variables 
  zar_brms(
    model_2,
    formula = Dens_L ~ 1 + (1 | PastLandUse),
    data = temp_indices %>% 
      mutate(PastLandUse = as.factor(PastLandUse)),
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_3,
    formula = DBH_med_L ~ 1 + (1 | PastLandUse),
    data = temp_indices %>% 
      mutate(PastLandUse = as.factor(PastLandUse)),
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  zar_brms(
    model_4,
    formula = SR_L ~ 1 + (1 | PastLandUse),
    data = temp_indices %>% 
      mutate(PastLandUse = as.factor(PastLandUse)),
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "Intercept"),
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
    formula = mean_day_s ~ 1 + mean_day_con_s + PastLandUse + Age_s + (1 | tod) + (1 | Park),
    data = temp_indices %>% 
      ungroup() %>% 
      mutate(Park = as.factor(Park),
             PastLandUse = as.factor(PastLandUse),
             tod = as.factor(tod),
             Age_s = scale(Age),
             mean_s = scale(mean), 
             mean_con_s = scale(mean_con),
             SR_L_s = scale(SR_L), 
             DBH_med_L_s = scale(DBH_med_L), 
             Dens_L_s = scale(Dens_L)),
    family = gaussian(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    iter = 1000,
    chains = 4,
    cores = 4
  ),

  tar_target(
    model_list,
    list(model_1_brms_sample, model_2_brms_sample, model_3_brms_sample, 
         model_4_brms_sample, model_5_brms_sample) %>%
      setNames(., c('model_1', 'model_2', 'model_3', 'model_4', 'model_5'))
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
  tar_target(
    model_diag_plots,
    model_diagnostics(model_list),
    pattern = map(model_list), 
    iteration = "list"
  )
  
  
)