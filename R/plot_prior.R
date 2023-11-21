plot_prior <- function(model_list, xmin, xmax, model_name){
  
  mod <- model_list[[1]]
  
  # how to sequence data
  seq_d <- seq(xmin, xmax, by = 0.5)
  
  # pull variables from model 
  vars <- colnames(mod$data)[-1]
  
  # identify categorical variables 
  factors <- names(Filter(is.factor,mod$data[-1]))
  
  # identify numeric variables 
  numeric <- names(Filter(is.numeric,mod$data[-1]))
  
figlist <- lapply(numeric, function(x){
    
    df <- data.frame(matrix(ncol=length(vars),nrow=0, dimnames=list(NULL, vars)))
    
    df %<>%
      summarise_at(x, ~seq_d) %>%
      bind_rows(df, .) %>%
      mutate(across(all_of(factors), as.factor)) %>% 
      mutate(across(all_of(numeric), ~ replace(., is.na(.), 0)))
    
    epred <- add_epred_draws(df, mod)
    
    ggplot(epred, aes(x = get(x), y = .epred)) +
      geom_line(aes(group = .draw), alpha = 0.1) +
      labs(y = "Predicted Temperature", x = x) +
      theme_classic()
    
  })

pdf(paste0('graphics/diagnostics/', model_name, '.pdf'))

do.call(grid.arrange, figlist)

dev.off()



}
