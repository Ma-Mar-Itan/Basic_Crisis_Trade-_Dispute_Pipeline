# Load the panel, split into groups, and build the differenced series.
# Produces: panel, groups, df_a, df_b, df_diff (shared with later steps).

raw    <- load_panel(cfg$data$path, cfg$data$sheet)
panel  <- build_pdata(raw, index = cfg$panel$index)
groups <- split_groups(panel, cfg$panel$group_a_countries)
df_a   <- groups$A
df_b   <- groups$B

# First difference of ln_gdpc (per country), used by stationarity & post-
# stationarity models.
df_diff <- add_first_difference(raw, "ln_gdpc")

message("Loaded panel: ", nrow(panel), " obs | Group A: ", nrow(df_a),
        " | Group B: ", nrow(df_b))
