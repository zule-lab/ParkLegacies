create_figure_1 <- function(){
  
  dagified <- dagify(
    cooling ~ tree_size,
    cooling ~ tree_diversity,
    cooling ~ tree_density,
    tree_density ~ plu,
    tree_diversity ~ age + species_comp,
    tree_size ~ age + carbon + nitrogen + metals + tree_diversity, 
    species_comp ~ carbon + nitrogen + metals, 
    carbon ~ plu, 
    nitrogen ~ plu, 
    metals ~ plu,
    labels = c(
      "cooling" = "Temperature",
      "tree_size" = "Tree Size",
      "tree_diversity" = "Tree Diversity",
      "tree_density" = "Tree Density",
      "age" = "Stand Age", 
      "plu" = "Past Land Use",
      "carbon" = "Carbon", 
      "nitrogen" = "Nitrogen", 
      "metals" = "Heavy Metals",
      "species_comp" = "Species \nComposition"
    ),
    exposure = 'plu',
    outcome = 'cooling',
    coords = list(x = c(cooling = 0, tree_density = -1, tree_size = 0, tree_diversity = 1, age = 0.5, 
                        nitrogen = -0.5, species_comp = 0.5, metals = 1.25, carbon = 0, plu = 0),
                  y = c(cooling = 3, tree_density = 1.5, tree_size = 2, tree_diversity = 2.5, age = 2, 
                        nitrogen = 1, species_comp = 1, metals = 1.5, carbon = 0.5, plu = 0))) %>%
    tidy_dagitty() %>%
    mutate(status = case_when(name == "cooling" ~ 'outcome',
                              name == "plu" ~ 'exposure',
                              name == "tree_size" ~ 'exposure',
                              name == "tree_diversity" ~ 'exposure',
                              name == "tree_density" ~ 'exposure',
                              .default = 'NA'))
  
  dagified <- shorten_dag_arrows(dagified, proportion = 0.2)
  
  
  i <- ggplot(dagified, aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_dag() + 
    geom_dag_point(aes(color = status), size = 25) +
    geom_dag_label_repel(aes(label = label, fill = status),
                         color = "white", fontface = "bold", size = 10, nudge_x = -1) +
    geom_dag_edges(aes(x = xstart, y = ystart), edge_width = 1.5) + 
    scale_fill_manual(values = c('grey', 'grey', 'grey')) + 
    scale_colour_manual(values = c('grey', 'grey', 'grey')) + 
    theme(legend.position = 'none')
  
  
  ggsave('graphics/figure_1.png', plot = i, width = 15, height = 15, units = "in")
  
}


shorten_dag_arrows <- function(tidy_dag, proportion){
  # Update underlying ggdag object
  tidy_dag$data <- dplyr::mutate(tidy_dag$data, 
                                 xend = (1-proportion/2)*(xend - x) + x, 
                                 yend = (1-proportion/2)*(yend - y) + y,
                                 xstart = (1-proportion/2)*(x - xend) + xend,
                                 ystart = (1-proportion/2)*(y-yend) + yend)
  return(tidy_dag)
}
