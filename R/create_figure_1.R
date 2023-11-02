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
  
  dagified_2 <- dagify(
    cooling ~ past_land_use,
    vis_min ~ past_land_use + cooling,
    recent_imm ~ past_land_use + cooling,
    edu ~ past_land_use + cooling + income,
    income ~ past_land_use + cooling + vis_min + recent_imm,
    labels = c(
      "cooling" = "Cooling\n Benefit",
      "past_land_use" = "Past Land\n Use",
      "vis_min" = "Visible\n Minorities (%)",
      "recent_imm" = "Recent\n Immigrants (%)",
      "edu" = "University\n Educated (%)",
      "income" = "Median\n Income"
    ),
    exposure = 'cooling',
    outcome = 'income',
    coords = list(x = c(past_land_use = 0, vis_min = -1, cooling = 0, recent_imm = 0.5, edu = 1, income = 0),
                  y = c(past_land_use = 1, vis_min = 0, cooling = 0, recent_imm = 0, edu = 0, income = -1))) %>%
    tidy_dagitty() %>%
    mutate(status = case_when(name == "income" ~ 'outcome',
                              name == "cooling" ~ 'exposure',
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
  
  s <- ggplot(dagified_2, aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_dag() + 
    geom_dag_point(aes(color = status)) +
    geom_dag_label_repel(aes(label = label, fill = status),
                         color = "white", fontface = "bold") +
    geom_dag_edges() + 
    scale_fill_manual(values = c('darkseagreen', 'grey', 'lightskyblue')) + 
    scale_colour_manual(values = c('darkseagreen', 'grey', 'lightskyblue')) + 
    theme(legend.position = 'none')
  
  t <- i | s
  
  ggsave('graphics/figure_1.png', plot = t, width = 15, height = 8, units = "in")
  
}
