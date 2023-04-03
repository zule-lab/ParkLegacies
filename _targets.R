# === Targets -------------------------------------------------------------
# Framework by Alec L. Robitaille



# Source ------------------------------------------------------------------
library(targets)
tar_source('R')



# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')


# Renv --------------------------------------------------------------------
activate()
snapshot()
restore()


# Variables ---------------------------------------------------------------


# Scripts -----------------------------------------------------------------
tar_source('scripts')


# Targets: all ------------------------------------------------------------
# Automatically grab all the "park" lists above
lapply(grep('target', ls(), value = TRUE), get)