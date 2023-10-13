data_generative <- function(){
  
  # we predict previously forested will have highest tree density,
  # agricultural will have lowest
  data_for <- data.frame(plu = 'forested', dens = rnorm(1000, mean = 0, sd = 1000))
  data_ag <- data.frame(plu = 'agricultural', dens = rnorm(1000, mean = 3000, sd = 1000))
  data_ind <- data.frame(plu = 'industrial', dens = rnorm(1000, mean = 5000, sd = 1000))
  
  # we predict that tree size and tree richness will be lowest in industrial sites and highest 
  # in forested sites
  
  data_for$size <- rnorm(1000, mean = 20, sd = 5)
  data_ag$size <- rnorm(1000, mean  = 15, sd = 5)
  data_ind$size <- rnorm(1000, mean = 10, sd = 5)
  
  data_for$div <- rnorm(1000, mean = 12, sd = 2)
  data_ag$div <- rnorm(1000, mean = 10, sd = 2)
  data_ind$div <- rnorm(1000, mean = 8, sd = 2)
   

  
  
  
}