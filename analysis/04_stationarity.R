# Panel unit-root battery (levels + first difference) and per-country ADF.

unit_root <- run_panel_unit_root(
  df_diff,
  variables = cfg$stationarity$variables,
  tests     = cfg$stationarity$panel_tests,
  exo_type  = cfg$stationarity$exo,
  lag_value = cfg$stationarity$lags
)
print(unit_root[, c("variable", "test", "statistic", "p_value", "conclusion_5pct")],
      n = Inf)
save_table_csv(as.data.frame(unit_root), "panel_unit_root.csv")

adf <- suppressWarnings(
  run_country_adf(df_diff, variables = c("ln_trade", "ln_cti", "ln_gdpc"))
)
save_table_csv(as.data.frame(adf), "adf_by_country.csv")
message("Stationarity tables written.")
