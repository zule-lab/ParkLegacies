data_generative <- function(){
  
  # past land-use 
  # generate 3000 observations of three categories of land-use
  N <- 3000

  df <- data.frame(plu = rep(c(1, 2, 3), 1000)) %>%
    mutate(plu = case_when(plu == 1 ~ 'Forested',
                           plu == 2 ~ 'Agricultural',
                           plu == 3 ~ 'Industrial'))
           # we predict previously forested will have highest tree density,
           # agricultural will have lowest
           #dens = case_when(plu == 1 ~ rnorm(., mean = )))
    
   

  
  
  
}