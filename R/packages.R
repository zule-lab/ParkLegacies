# === Packages ------------------------------------------------------------
# Framework by Alec L. Robitaille

library(conflicted)

library(targets)
library(tarchetypes)
library(renv)

library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

library(ggplot2)
library(ggdag)
library(ggspatial)
library(ggdist)
library(grid)
library(patchwork)
library(gridExtra)

library(datetime)

library(iNEXT)
library(zarg)

library(suncalc)

library(xfun)
library(janitor)

library(knitr)
library(rmarkdown)

library(downloader)

suppressPackageStartupMessages({
  library(sf)
  library(stars)
  library(terra)
  library(osmdata)
  
  library(qs)
  library(bit64)
  
  library(brms)
  library(cmdstanr)
  library(tidybayes)
  library(bayesplot)
})

conflicts_prefer(renv::autoload, 
                 renv::embed, 
                 renv::history, 
                 base::`:`,
                 base::`%in%`,
                 dplyr::filter,
                 dplyr::first,
                 base::match,
                 .quiet = T) 