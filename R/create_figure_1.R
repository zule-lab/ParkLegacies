create_figure_1 <- function(){
  
  dagified <- dagify(
    cooling ~ tree_size,
    cooling ~ tree_diversity,
    cooling ~ tree_density,
    tree_size ~ soil + age,
    tree_diversity ~ soil + age,
    tree_density ~ past_land_use,
    soil ~ past_land_use,
    labels = c(
      "cooling" = "Cooling",
      "tree_size" = "Tree Size",
      "tree_diversity" = "Tree Diversity",
      "tree_density" = "Tree Density",
      "soil" = "Soil",
      "age" = "Age", 
      "past_land_use" = "Past Land Use"
    ),
    exposure = 'past_land_use',
    outcome = 'cooling',
    coords = list(x = c(cooling = 0, tree_density = -1, tree_size = 0, tree_diversity = 1, age = 1, soil = 0, past_land_use = 0),
                  y = c(cooling = 3, tree_density = 2, tree_size = 2, tree_diversity = 2, age = 1, soil = 1, past_land_use = 0))) %>%
    tidy_dagitty() %>%
    mutate(status = case_when(name == "cooling" ~ 'outcome',
                              name == "past_land_use" ~ 'exposure',
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
    scale_fill_manual(values = c('darkseagreen', 'grey', 'lightskyblue')) + 
    scale_colour_manual(values = c('darkseagreen', 'grey', 'lightskyblue')) + 
    theme(legend.position = 'none')
  
  
  ggsave('graphics/figure_1.png', plot = i, width = 15, height = 12, units = "in")
  
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
