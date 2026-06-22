# Estimate OLS / FE / RE for both groups, with robust SE and criteria.
# Writes coefficient tables to output/tables.

estimate_group <- function(data, label) {
  ols <- fit_ols(data)
  fe  <- fit_fe(data)
  re  <- fit_re(data, method = "walhus")

  # Robust (Driscoll-Kraay for FE, White for OLS), as in the legacy scripts.
  fe_robust  <- robust_coeftest(fe,  function(m) driscoll_kraay(m, "HC1"))
  ols_robust <- robust_coeftest(ols, function(m) white_hc(m, "HC1"))

  message("== Group ", label, " ==")
  print(coef(fe))
  list(ols = ols, fe = fe, re = re,
       fe_robust = fe_robust, ols_robust = ols_robust,
       criteria_ols = info_criteria(ols),
       criteria_fe  = fe_manual_criteria(fe),
       stats        = panel_stats(re))
}

models_a <- estimate_group(df_a, "A")
models_b <- estimate_group(df_b, "B")

# Persist FE coefficient tables.
fe_coef_tbl <- function(m) as.data.frame(coef(m$fe_robust))
save_table_csv(cbind(term = rownames(fe_coef_tbl(models_a)), fe_coef_tbl(models_a)),
               "fe_groupA_robust.csv")
save_table_csv(cbind(term = rownames(fe_coef_tbl(models_b)), fe_coef_tbl(models_b)),
               "fe_groupB_robust.csv")
message("Model tables written to ", cfg$output$tables_dir)
