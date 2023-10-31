model_diagnostics <- function(model_fit, model_name){
  
  pdf(paste0('graphics/diagnostics/', model_name, '.pdf'))
  
  plot(model_fit)
  
  ac <- as_draws_df(model_fit) %>% 
    mutate(chain = .chain) %>% 
    mcmc_acf(pars = vars(b_Intercept), lags = 35)
  
  plot(ac)
  
  coda::gelman.plot(as.mcmc(model_fit)[, "b_Intercept", ])
  
  dev.off()

}