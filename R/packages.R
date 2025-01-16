# === Packages ------------------------------------------------------------
# Framework by Alec L. Robitaille

suppressPackageStartupMessages({
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
  library(gt)
  
  library(downloader)
  
  library(sf)
  library(stars)
  library(terra)
  library(osmdata)
  
  library(qs)
  library(qs2)
  library(bit64)
  
  library(brms)
  library(cmdstanr)
  library(tidybayes)
  library(bayesplot)
  library(modelr)
  
})

conflicts_prefer(renv::autoload, 
                 renv::embed, 
                 renv::history, 
                 renv::modify,
                 renv::update,
                 renv::upgrade,
                 renv::init,
                 base::`:`,
                 base::`%in%`,
                 base::load,
                 base::remove,
                 base::is.double,
                 base::xor,
                 base::order,
                 base::rank,
                 utils::hashtab,
                 data.table::transpose,
                 data.table::setattr,
                 dplyr::filter,
                 dplyr::first,
                 dplyr::between,
                 dplyr::combine,
                 dplyr::lag,
                 dplyr::last,
                 dplyr::setdiff,
                 dplyr::setequal,
                 dplyr::union,
                 dplyr::symdiff,
                 base::match,
                 brms::ar,
                 brms::autocor,
                 brms::dstudent_t,
                 brms::pstudent_t,
                 brms::qstudent_t,
                 brms::rstudent_t,
                 bayesplot::rhat,
                 terra::spin,
                 terra::crosstab,
                 terra::area,
                 terra::depth,
                 terra::intersect,
                 terra::extract,
                 terra::shift,
                 terra::project,
                 janitor::chisq.test,
                 janitor::fisher.test,
                 rmarkdown::run,
                 xfun::attr,
                 .quiet = T) #
