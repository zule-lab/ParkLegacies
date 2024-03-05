# === Targets -------------------------------------------------------------
# Framework by Alec L. Robitaille



# Source ------------------------------------------------------------------
library(targets)
tar_source('R')



# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')
options(timeout=100)

# Renv --------------------------------------------------------------------
activate()
snapshot()
restore()


# Variables ---------------------------------------------------------------


# Scripts -----------------------------------------------------------------
tar_source('scripts')


# Targets: all ------------------------------------------------------------
# Automatically grab all the "park" lists above
#lapply(grep('target', ls(), value = TRUE), get)

c(target_models, target_manuscript_figures, target_park_prep, target_park_sampling, target_redo_canopy, target_temp_prep, target_tree_prep)