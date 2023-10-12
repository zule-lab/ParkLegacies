# === Packages ------------------------------------------------------------
# Framework by Alec L. Robitaille

library(conflicted)

library(targets)
library(tarchetypes)
library(renv)
library(qs)

library(sf)
library(stars)
library(terra)
library(osmdata)

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

library(datetime)

library(iNEXT)

library(suncalc)

library(xfun)
library(janitor)

library(knitr)
library(rmarkdown)

library(downloader)
suppressMessages(library(bit64))

conflicts_prefer(renv::autoload, 
                 renv::embed, 
                 renv::history, 
                 base::`:`,
                 base::`%in%`,
                 dplyr::filter,
                 dplyr::first,
                 base::match,
                 .quiet = T) 