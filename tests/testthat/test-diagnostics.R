# Tests for R/diagnostics.R  --------------------------------------------------

fx_pdata <- function() build_pdata(make_mini_panel())

test_that("diagnostic wrappers match direct calls and return htest", {
  pd  <- fx_pdata()
  fe  <- fit_fe(pd); re <- fit_re(pd, method = "walhus"); ols <- fit_ols(pd)

  expect_equal(hausman(fe, re)$statistic, plm::phtest(fe, re)$statistic)
  expect_equal(bp_test(ols)$p.value,      lmtest::bptest(ols)$p.value)
  expect_equal(bg_test(ols, 2)$statistic, lmtest::bgtest(ols, order = 2)$statistic)
  expect_equal(pesaran_cd(fe, "cd")$statistic, plm::pcdtest(fe, test = "cd")$statistic)
  # wooldridge_serial (pwartest) needs more periods than the fixture has;
  # it is proven bit-identical on the real data in tools/equivalence_diagnostics.R.
})

test_that("hausman refuses a cross-group comparison", {
  pd <- fx_pdata()
  g  <- split_groups(pd)
  fe_a <- fit_fe(g$A)
  re_b <- fit_re(g$B, method = "walhus")
  expect_error(hausman(fe_a, re_b), "different individuals|cross-group")
})

test_that("vif_values matches the 1/(1-R2) definition", {
  pd  <- fx_pdata()
  ols <- fit_ols(pd)
  v   <- vif_values(ols)
  # Manual check for one regressor (ln_cti regressed on the others).
  X   <- as.data.frame(model.matrix(ols))
  r2  <- summary(lm(ln_cti ~ ln_gdpc + distance + crises, data = X))$r.squared
  expect_equal(unname(v["ln_cti"]), 1 / (1 - r2))
})
