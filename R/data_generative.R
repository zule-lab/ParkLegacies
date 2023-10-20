data_generative <- function(){

  # we predict previously forested will have highest tree density,
  # agricultural will have lowest
  # we predict that tree size and tree richness will be lowest in industrial sites and highest 
  # in forested sites
  
  avgs_dens <- c("forested" = 3000, "agricultural" = 2000, "industrial" = 2500)
  avgs_size <- c("forested" = 20, "agricultural" = 15, "industrial" = 13)
  avgs_richness <- c("forested" = 10, "agricultural" = 9, "industrial" = 8)
  
  # relative influence of each relationship on temp
  b_D <- 2.5
  b_S <- 3
  b_R <- 1
  
  gen <- tibble(group = rep(names(avgs_dens), each = 100),
         mu_dens = avgs_dens[group],
         dens = rnorm(length(mu_dens), mean = mu_dens, sd = 1000),
         dens_s = scale(dens),
         mu_size = avgs_size[group],
         size = rnorm(length(mu_size), mean = mu_size, sd = 5),
         size_s = scale(size),
         mu_richness = avgs_richness[group],
         richness = rnorm(length(mu_richness), mean = mu_richness, sd = 2),
         richness_s = scale(richness),
         temp = rnorm(length(mu_dens), b_D*dens_s + b_S*size_s + b_R*richness_s + 30, 0.5))

  
  
  
}