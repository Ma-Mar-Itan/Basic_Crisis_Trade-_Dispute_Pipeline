# Project IBR — Qatar Trade & Blockade Panel Analysis

Reproducible econometric analysis of the **2017 Qatar diplomatic crisis** and its
effect on bilateral trade, using a gravity model on a country–year panel.

## Model

```
ln_trade ~ ln_cti + ln_gdpc + distance + crises
```

Estimated by Pooled OLS, Fixed Effects, Random Effects (walhus/amemiya/nerlove),
and Correlated Random Effects, with Driscoll–Kraay robust standard errors. The
panel is split into **Group A** (the blockading states: Saudi, UAE, Bahrain,
Egypt) and **Group B** (the rest).

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

## Getting started

1. Place `Qatar_Data_V.1.0.xlsx` in `data/raw/` (see [`data/README.md`](data/README.md)).
2. Install dependencies (see `DESCRIPTION`).
3. Run the full analysis:

   ```r
   Rscript run_analysis.R
   ```

4. Run the tests:

   ```r
   Rscript tools/run_tests.R
   ```

## Status

The entire analysis has been refactored into clean, documented, tested functions
and **proven bit-identical** to the original scripts on the real dataset. The
equivalence harnesses in `tools/` (load, models, diagnostics, stationarity,
descriptives) compare every clean function against the verbatim legacy logic —
max absolute difference **0** across coefficients, robust SE, p-values, R²,
AIC/BIC/HQ, Hausman, unit-root, heteroskedasticity, CD, and descriptives (VIF
matches `car::vif` to 1e-14). The original scripts have been removed; they remain
in git history.
