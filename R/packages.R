# === Packages ------------------------------------------------------------
# Framework by Alec L. Robitaille

library(conflicted)

library(targets)
library(tarchetypes)
library(renv)
library(qs)
library(fnmate)
conflict_prefer('autoload', 'renv')
conflict_prefer('embed', 'renv')
conflict_prefer('history', 'renv')

library(sf)
library(stars)
library(terra)
library(osmdata)

library(data.table)
library(dplyr)
library(tidyr)
library(purrr)

library(ggplot2)
library(ggspatial)
library(ggdist)

library(knitr)
library(rmarkdown)

library(downloader)
library(bit64)
conflict_prefer(":", "base")