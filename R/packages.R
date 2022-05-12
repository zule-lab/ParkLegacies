# === Packages ------------------------------------------------------------
# Framework by Alec L. Robitaille

library(conflicted)

library(targets)
library(tarchetypes)
library(renv)
conflict_prefer('autoload', 'renv')
conflict_prefer('embed', 'renv')
conflict_prefer('history', 'renv')

library(sf)
library(stars)
library(terra)

library(data.table)
library(dplyr)

library(ggplot2)
library(ggspatial)
library(ggdist)

library(knitr)
library(rmarkdown)
