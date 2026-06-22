# Tests for R/models.R, R/robust.R, R/criteria.R  -----------------------------

fixture_pdata <- function() build_pdata(make_mini_panel())

test_that("fit_ols / fit_fe / fit_re return the expected classes", {
  pd <- fixture_pdata()
  expect_s3_class(fit_ols(pd), "lm")
  expect_s3_class(fit_fe(pd), "plm")
  expect_s3_class(fit_re(pd, method = "walhus"), "plm")
})

test_that("fit_fe equals a direct plm within call", {
  pd <- fixture_pdata()
  direct <- plm::plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises,
                     data = pd, index = c("country", "year"), model = "within")
  expect_equal(coef(fit_fe(pd)), coef(direct))
})

test_that("fit_re rejects an unknown method", {
  pd <- fixture_pdata()
  expect_error(fit_re(pd, method = "bogus"))
})

test_that("fit_cre adds the country-mean regressors", {
  # Use a single regressor so the between model is estimable on the small
  # fixture (6 countries). Full-spec CRE equivalence is proven on the real
  # data in tools/equivalence_models.R.
  pd  <- fixture_pdata()
  cre <- fit_cre(pd, regressors = "ln_cti")
  expect_s3_class(cre, "plm")
  expect_true("mean_ln_cti" %in% names(coef(cre)))
})

test_that("hannan_quinn matches the legacy HQ formula on an lm", {
  pd  <- fixture_pdata()
  m   <- fit_ols(pd)
  ll  <- as.numeric(logLik(m)); k <- attr(logLik(m), "df"); n <- nobs(m)
  expect_equal(hannan_quinn(m), -2 * ll + 2 * k * log(log(n)))
})

test_that("info_criteria agrees with base AIC/BIC", {
  pd <- fixture_pdata()
  m  <- fit_ols(pd)
  ic <- info_criteria(m)
  expect_equal(ic$AIC, AIC(m))
  expect_equal(ic$BIC, BIC(m))
})

test_that("fe_manual_criteria reproduces the residual-based formula", {
  pd  <- fixture_pdata()
  fe  <- fit_fe(pd)
  res <- residuals(fe); n <- length(res); s2 <- mean(res^2)
  ll  <- -n / 2 * (log(2 * pi) + 1 + log(s2)); k <- length(coef(fe))
  out <- fe_manual_criteria(fe)
  expect_equal(out$AIC, -2 * ll + 2 * k)
  expect_equal(out$HQC, -2 * ll + 2 * k * log(log(n)))
})
