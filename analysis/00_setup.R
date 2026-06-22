# Setup: load packages, source the R/ function modules, read config.
# Sourced first by run_analysis.R.

suppressMessages({
  library(plm); library(dplyr); library(readxl)
  library(sandwich); library(lmtest); library(tseries)
})

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

cfg <- load_config()
message("Setup complete: ", length(ls(pattern = "^(fit_|run_|load_|split_)")),
        " core functions available.")
