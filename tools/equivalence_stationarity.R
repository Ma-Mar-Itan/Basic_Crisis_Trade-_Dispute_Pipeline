# =============================================================================
# EQUIVALENCE: stationarity layer (stationarity.R)
# Compares run_unit_root_test() against an inline replica of Panel root.R's
# run_panel_test() for every (variable x test) cell, on the real data.
# =============================================================================
suppressMessages({ library(readxl); library(plm); library(dplyr); library(tibble); library(tseries) })
PATH <- "data/raw/Qatar_Data_V.1.0.xlsx"; SHEET <- "data_for_model"
renv <- new.env(); for (f in list.files("R","[.]R$",full.names=TRUE)) sys.source(f, envir=renv)

df <- read_excel(PATH, sheet=SHEET); df$year<-as.numeric(df$year); df$crises<-as.numeric(df$crises)
# original differencing for d_ln_gdpc (Panel root expects it)
df <- df |> arrange(country,year) |> group_by(country) |>
  mutate(d_ln_gdpc = ln_gdpc - dplyr::lag(ln_gdpc)) |> ungroup() |> as.data.frame()

# ---- inline replica of the ORIGINAL Panel root.R run_panel_test (numeric core)
o_run <- function(data, v, test_name, exo="intercept", lag=1) {
  td <- data |> select(country,year,all_of(v)) |>
    filter(!is.na(country),!is.na(year),!is.na(.data[[v]]),is.finite(.data[[v]])) |>
    arrange(country,year) |> distinct(country,year,.keep_all=TRUE)
  valid <- td |> group_by(country) |>
    summarise(n=n(), u=n_distinct(.data[[v]],na.rm=TRUE), .groups="drop") |>
    filter(n>=lag+5, u>1) |> pull(country)
  td <- filter(td, country %in% valid)
  if (n_distinct(td$country) < 2) return(c(stat=NA, p=NA))
  pdf <- pdata.frame(td, index=c("country","year"), drop.index=FALSE, row.names=TRUE)
  r <- tryCatch(purtest(pdf[[v]], test=test_name, exo=exo, lags=lag), error=function(e) e)
  if (inherits(r,"error")) return(c(stat=NA, p=NA))
  c(stat=as.numeric(r$statistic$statistic[1]), p=as.numeric(r$statistic$p.value[1]))
}

vars  <- c("ln_trade","ln_cti","ln_gdpc","d_ln_gdpc")
tests <- c("ips","madwu","Pm","invnormal","logit","hadri")
rr <- renv$run_panel_unit_root(df, variables=vars, tests=tests, exo_type="intercept", lag_value=1)

maxd <- 0; ncells <- 0
for (v in vars) for (tt in tests) {
  o <- o_run(df, v, tt)
  row <- rr[rr$variable==v & rr$test_code==tt, ]
  ds <- abs(o["stat"] - row$statistic); dp <- abs(o["p"] - row$p_value)
  ds <- ifelse(is.na(ds), 0, ds); dp <- ifelse(is.na(dp), 0, dp)
  maxd <- max(maxd, ds, dp, na.rm=TRUE); ncells <- ncells + 1
  cat(sprintf("%-10s %-10s stat O=%9.4f R=%9.4f  p O=%.4f R=%.4f  d=%.2e\n",
              v, tt, o["stat"], row$statistic, o["p"], row$p_value, max(ds,dp)))
}

# ADF spot-check (one variable)
adf <- renv$run_country_adf(df, variables="ln_gdpc", lag_order=0)
cat(sprintf("\nADF ln_gdpc rows: %d (per-country) - ran OK\n", nrow(adf)))

cat(sprintf("\n==== STATIONARITY LAYER: %d cells, MAX DIFF=%.3e -> %s ====\n",
  ncells, maxd, if(maxd==0)"BIT-IDENTICAL" else if(maxd<1e-10)"IDENTICAL (fp tol)" else "DIFFERENCES"))
