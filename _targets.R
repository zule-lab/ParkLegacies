# === Targets -------------------------------------------------------------
# Framework by Alec L. Robitaille



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



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