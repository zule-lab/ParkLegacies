# === Targets -------------------------------------------------------------
# Framework by Alec L. Robitaille



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')


# Variables ---------------------------------------------------------------


# Scripts -----------------------------------------------------------------
source(file.path('scripts', 'park-prep.R'))
source(file.path('scripts', 'park-historical-boundaries.R'))
source(file.path('scripts', 'park-sampling.R'))
source(file.path('scripts', 'park-soil-sampling.R'))

# Targets: all ------------------------------------------------------------
# Automatically grab all the "park" lists above
lapply(grep('park', ls(), value = TRUE), get)