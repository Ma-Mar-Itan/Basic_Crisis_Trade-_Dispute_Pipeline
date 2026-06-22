# =============================================================================
# Single entry point for the full analysis.
#
#   Rscript run_analysis.R
#
# Runs the numbered analysis/ steps in order, sharing one environment, and
# writes all tables/figures to output/. Replaces the implicit "run these
# scripts in the right order" coupling of the legacy code.
# =============================================================================

steps <- c(
  "analysis/00_setup.R",
  "analysis/01_load_split.R",
  "analysis/02_models.R",
  "analysis/03_diagnostics.R",
  "analysis/04_stationarity.R",
  "analysis/05_descriptives_plots.R"
)

for (s in steps) {
  message("\n>>> ", s)
  source(s)
}

message("\nAnalysis complete. See output/ for tables and figures.")
