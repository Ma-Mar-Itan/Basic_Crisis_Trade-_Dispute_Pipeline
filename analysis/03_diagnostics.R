# Diagnostics for both groups: Hausman, Breusch-Pagan, Wooldridge, Pesaran CD.
# Uses the models estimated in 02_models.R.

diagnose_group <- function(m, data, label) {
  haus <- hausman(m$fe, m$re)            # guarded against cross-group misuse
  bp   <- bp_test(m$ols)
  cd   <- pesaran_cd(m$fe, "cd")
  serial <- tryCatch(wooldridge_serial(m$fe), error = function(e) NULL)

  data.frame(
    group              = label,
    hausman_stat       = unname(haus$statistic),
    hausman_p          = haus$p.value,
    bp_stat            = unname(bp$statistic),
    bp_p               = bp$p.value,
    cd_stat            = unname(cd$statistic),
    cd_p               = cd$p.value,
    wooldridge_p       = if (is.null(serial)) NA_real_ else serial$p.value
  )
}

diagnostics <- rbind(
  diagnose_group(models_a, df_a, "A"),
  diagnose_group(models_b, df_b, "B")
)
print(diagnostics)
save_table_csv(diagnostics, "diagnostics_by_group.csv")
message("Diagnostics written.")
