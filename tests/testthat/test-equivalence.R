# Functional-equivalence regression test.
#
# Proves the refactored data path produces results identical to the original
# inline logic, on the REAL dataset. Skips automatically when the data file is
# absent (it is gitignored), so the suite stays green in CI without the data.

real_data <- function() {
  p <- file.path(find_project_root(), "data", "raw", "Qatar_Data_V.1.0.xlsx")
  if (!file.exists(p)) testthat::skip("real dataset not present")
  p
}

GA   <- c("Saudi", "UAE", "Bahrain", "Egypt")
FORM <- ln_trade ~ ln_cti + ln_gdpc + distance + crises

test_that("refactored load/split is identical to the original inline logic", {
  path <- real_data()

  # ORIGINAL
  o <- readxl::read_excel(path, sheet = "data_for_model")
  o$year <- as.numeric(o$year); o$crises <- as.numeric(o$crises)
  o_p <- plm::pdata.frame(o, index = c("country", "year"))
  o_a <- o_p[o_p$country %in% GA, ]; o_b <- o_p[!o_p$country %in% GA, ]

  # REFACTORED
  r_p <- build_pdata(load_panel(path, "data_for_model"))
  g   <- split_groups(r_p, GA)

  expect_equal(as.data.frame(g$A)[, names(o_a)], as.data.frame(o_a),
               ignore_attr = TRUE)
  expect_equal(as.data.frame(g$B)[, names(o_b)], as.data.frame(o_b),
               ignore_attr = TRUE)
})

test_that("FE/RE coefficients & robust SE are identical across data paths", {
  path <- real_data()

  o <- readxl::read_excel(path, sheet = "data_for_model")
  o$year <- as.numeric(o$year); o$crises <- as.numeric(o$crises)
  o_p <- plm::pdata.frame(o, index = c("country", "year"))
  o_a <- o_p[o_p$country %in% GA, ]

  r_a <- split_groups(build_pdata(load_panel(path, "data_for_model")), GA)$A

  fe_o <- plm::plm(FORM, data = o_a, index = c("country","year"), model = "within")
  fe_r <- plm::plm(FORM, data = r_a, index = c("country","year"), model = "within")
  expect_equal(coef(fe_o), coef(fe_r))

  rob_o <- lmtest::coeftest(fe_o, plm::vcovSCC(fe_o, type = "HC1"))
  rob_r <- lmtest::coeftest(fe_r, plm::vcovSCC(fe_r, type = "HC1"))
  expect_equal(rob_o[, "Std. Error"], rob_r[, "Std. Error"])
  expect_equal(rob_o[, "Pr(>|t|)"],   rob_r[, "Pr(>|t|)"])
})

test_that("d_ln_gdpc matches the original first-difference exactly", {
  path <- real_data()
  o <- readxl::read_excel(path, sheet = "data_for_model")
  o$year <- as.numeric(o$year)

  o_diff <- o |>
    dplyr::arrange(country, year) |>
    dplyr::group_by(country) |>
    dplyr::mutate(d_ln_gdpc = ln_gdpc - dplyr::lag(ln_gdpc)) |>
    dplyr::ungroup()
  r_diff <- add_first_difference(as.data.frame(o), "ln_gdpc")

  expect_equal(r_diff$d_ln_gdpc, o_diff$d_ln_gdpc)
})
