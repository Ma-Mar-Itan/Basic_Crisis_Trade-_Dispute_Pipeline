# Descriptive statistics and diagnostic plots.

if (requireNamespace("psych", quietly = TRUE)) {
  desc <- describe_by_country(raw)
  save_table_csv(desc, "descriptives_by_country.csv")
  message("Descriptives written.")
} else {
  message("psych not installed; skipping describe_by_country.")
}

# Residuals-vs-fitted heteroskedasticity plots (FE), per group.
if (requireNamespace("ggplot2", quietly = TRUE)) {
  save_plot_svg(
    plot_resid_fitted(models_a$fe,
                      "FE Residual Diagnostics: Group A"),
    "fe_residuals_fitted_A.svg")
  save_plot_svg(
    plot_resid_fitted(models_b$fe,
                      "FE Residual Diagnostics: Group B"),
    "fe_residuals_fitted_B.svg")
  message("Figures written to ", cfg$output$figures_dir)
} else {
  message("ggplot2 not installed; skipping plots.")
}
