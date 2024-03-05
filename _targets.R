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

# test

# Targets: all ------------------------------------------------------------
# Automatically grab all the "park" lists above
lapply(grep('target', ls(), value = TRUE), get)