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
      "cooling" = "Cooling\n Benefit",
      "tree_size" = "Tree\n Size",
      "tree_diversity" = "Tree\n Diversity",
      "tree_density" = "Tree\n Density",
      "soil" = "Soil",
      "age" = "Age", 
      "past_land_use" = "Past Land\n Use"
    ),
    exposure = 'past_land_use',
    outcome = 'cooling',
    coords = list(x = c(cooling = 0, tree_density = -1, tree_size = 0, tree_diversity = 1, age = 1, soil = 0, past_land_use = 0),
                  y = c(cooling = 3, tree_density = 2, tree_size = 2, tree_diversity = 2, age = 1, soil = 1, past_land_use = 0))) %>%
    tidy_dagitty() %>%
    mutate(status = case_when(name == "cooling" ~ 'outcome',
                              name == "past_land_use" ~ 'exposure',
                              .default = 'NA'))
  
  i <- ggplot(dagified, aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_dag() + 
    geom_dag_point(aes(color = status)) +
    geom_dag_label_repel(aes(label = label, fill = status),
                         color = "white", fontface = "bold") +
    geom_dag_edges() + 
    scale_fill_manual(values = c('darkseagreen', 'grey', 'lightskyblue')) + 
    scale_colour_manual(values = c('darkseagreen', 'grey', 'lightskyblue')) + 
    theme(legend.position = 'none')
  
  ggsave('graphics/figure_1.png', plot = i, width = 10, height = 8, units = "in")
  
}
