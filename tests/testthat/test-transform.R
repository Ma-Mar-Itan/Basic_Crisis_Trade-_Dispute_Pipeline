# Tests for R/transform.R  ----------------------------------------------------

test_that("add_first_difference yields one leading NA per country", {
  df  <- make_mini_panel()
  out <- add_first_difference(df, "ln_gdpc")

  expect_true("d_ln_gdpc" %in% names(out))
  # Row count is preserved (NA rows kept).
  expect_equal(nrow(out), nrow(df))

  # Exactly one NA per country (the first year).
  na_by_country <- tapply(out$d_ln_gdpc, out$country,
                          function(x) sum(is.na(x)))
  expect_true(all(na_by_country == 1L))
})

test_that("add_first_difference matches a manual diff for one country", {
  df  <- make_mini_panel()
  out <- add_first_difference(df, "ln_gdpc")

  saudi <- out[out$country == "Saudi", ]
  saudi <- saudi[order(saudi$year), ]
  manual <- c(NA, diff(saudi$ln_gdpc))
  expect_equal(saudi$d_ln_gdpc, manual)
})

test_that("add_first_difference does not mutate the input", {
  df <- make_mini_panel()
  before <- names(df)
  invisible(add_first_difference(df, "ln_gdpc"))
  expect_equal(names(df), before)          # no d_ln_gdpc leaked into caller
})

test_that("clean_panel_var preserves real years when given a factor column (H-1)", {
  df <- make_mini_panel()
  df$year <- as.factor(df$year)            # simulate a pdata.frame/pseries year
  cleaned <- clean_panel_var(df, "ln_gdpc", min_T = 6)
  # Must be actual years (2010..), NOT factor level codes (1, 2, 3..).
  expect_true(all(cleaned$year >= 2010 & cleaned$year <= 2019))
  expect_setequal(unique(cleaned$year), 2010:2019)
})

test_that("add_first_difference orders correctly with a factor year (M-2)", {
  df <- make_mini_panel()
  df$year <- as.factor(df$year)
  out <- add_first_difference(df, "ln_gdpc")
  saudi <- out[out$country == "Saudi", ]
  saudi <- saudi[order(saudi$year), ]
  expect_equal(saudi$d_ln_gdpc, c(NA, diff(saudi$ln_gdpc)))
  expect_type(out$year, "integer")         # year normalized to integer
})

test_that("clean_panel_var drops short countries and non-finite values", {
  df <- make_mini_panel()
  # Japan gets only 3 obs -> should be dropped at min_T = 6.
  df <- df[!(df$country == "Japan" & df$year > 2012), ]
  cleaned <- clean_panel_var(df, "ln_gdpc", min_T = 6)

  expect_false("Japan" %in% as.character(cleaned$country))
  expect_true("value" %in% names(cleaned))
  expect_true(all(is.finite(cleaned$value)))
})
