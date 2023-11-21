model_diagnostics <- function(model_list){
  
  model_name <- names(model_list)
  
  model_fit <- model_list[[1]]
  
  pdf(paste0('graphics/diagnostics/', model_name, '_diagnostics.pdf'))
  
  # basic model fit plots
  plot(model_fit)
  
  # Gelman plot
  ac <- as_draws_df(model_fit) %>% 
    mutate(chain = .chain) %>% 
    mcmc_acf(pars = vars(b_Intercept), lags = 35)
  
  plot(ac)
  
  coda::gelman.plot(as.mcmc(model_fit)[, "b_Intercept", ])
  
  # posterior check
  pp_check(model_fit, ndraws = 100)
  
  dev.off()

}
