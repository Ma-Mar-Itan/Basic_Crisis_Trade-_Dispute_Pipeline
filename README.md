# Project IBR 

Reproducible econometric analysis for project IBR


## Structure

```
R/            clean, tested functions (load, transform, models, robust SE,
              criteria, diagnostics, stationarity, descriptives, plots, export)
analysis/     numbered pipeline runners (00_setup .. 05_descriptives_plots)
run_analysis.R single entry point for the full study
tests/        testthat unit + equivalence tests
tools/        equivalence harnesses + test runner
config/       config.yml — paths, group definitions, model formula
data/         data/raw/ holds the input xlsx (not committed; see data/README.md)
output/       generated tables + figures (gitignored)
```

