# Tests for R/stationarity.R  -------------------------------------------------

test_that("run_unit_root_test returns a single well-formed row", {
  df  <- make_mini_panel()
  row <- run_unit_root_test(df, "ln_gdpc", "ips", "intercept", 1)
  expect_s3_class(row, "tbl_df")
  expect_equal(nrow(row), 1L)
  expect_true(all(c("variable", "test_code", "statistic", "p_value",
                    "null_hypothesis", "conclusion_5pct") %in% names(row)))
  expect_equal(row$variable, "ln_gdpc")
})

test_that("run_unit_root_test returns an explicit failure row, never throws", {
  df <- make_mini_panel()
  df <- df[df$country == "Saudi", ]          # only one country -> < 2 valid
  row <- run_unit_root_test(df, "ln_gdpc", "ips", "intercept", 1)
  expect_equal(nrow(row), 1L)
  expect_true(is.na(row$statistic))
  expect_match(row$error, "two valid countries")
})

test_that("run_panel_unit_root produces one row per variable x test", {
  df  <- make_mini_panel()
  out <- run_panel_unit_root(df, variables = c("ln_trade", "ln_gdpc"),
                             tests = c("ips", "madwu"), lag_value = 1)
  expect_equal(nrow(out), 4L)                 # 2 vars x 2 tests
  expect_setequal(unique(out$variable), c("ln_trade", "ln_gdpc"))
})

test_that("run_country_adf returns per-country rows", {
  df  <- make_mini_panel()
  out <- suppressWarnings(run_country_adf(df, variables = "ln_gdpc", min_T = 6))
  expect_true(all(c("country", "variable", "statistic", "p_value",
                    "decision_5pct") %in% names(out)))
  expect_setequal(as.character(out$country),
                  c("Saudi", "UAE", "Bahrain", "Egypt", "USA", "Japan"))
})
