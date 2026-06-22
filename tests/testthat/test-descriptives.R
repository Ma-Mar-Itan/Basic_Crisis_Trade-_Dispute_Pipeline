# Tests for R/descriptives.R, R/plots.R, R/export.R  --------------------------

test_that("panel_stats matches the legacy panel_stats_plm formula", {
  pd <- build_pdata(make_mini_panel())
  m  <- fit_re(pd, method = "walhus")
  y  <- m$model[[1]]; u <- residuals(m)
  ps <- panel_stats(m)
  expect_equal(ps$Mean_dep_var, mean(y, na.rm = TRUE))
  expect_equal(ps$Sum_sq_resid, sum(u^2))
  expect_equal(ps$SE_regression, sqrt(sum(u^2) / df.residual(m)))
})

test_that("save_table_csv writes a readable round-trip", {
  d    <- data.frame(a = 1:3, b = c("x", "y", "z"))
  path <- save_table_csv(d, "test_roundtrip.csv")
  on.exit(unlink(path), add = TRUE)
  expect_true(file.exists(path))
  back <- utils::read.csv(path)
  expect_equal(back$a, d$a)
})

test_that("plot_resid_fitted returns a ggplot", {
  skip_if_not_installed("ggplot2")
  pd <- build_pdata(make_mini_panel())
  m  <- fit_fe(pd)
  expect_s3_class(plot_resid_fitted(m), "ggplot")
})
