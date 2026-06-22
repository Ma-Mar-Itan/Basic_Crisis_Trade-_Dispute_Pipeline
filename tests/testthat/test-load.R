# Tests for R/load.R  ---------------------------------------------------------

test_that("build_pdata returns a pdata.frame indexed on country/year", {
  df  <- make_mini_panel()
  pdf <- build_pdata(df)
  expect_s3_class(pdf, "pdata.frame")
  idx <- attr(pdf, "index")
  expect_equal(names(idx), c("country", "year"))
})

test_that("split_groups puts exactly the four blockading states in Group A", {
  pdf    <- build_pdata(make_mini_panel())
  groups <- split_groups(pdf)

  a_countries <- sort(unique(as.character(groups$A$country)))
  b_countries <- sort(unique(as.character(groups$B$country)))

  expect_setequal(a_countries, c("Bahrain", "Egypt", "Saudi", "UAE"))
  expect_setequal(b_countries, c("Japan", "USA"))
  # A and B partition the panel with no overlap and no loss.
  expect_equal(nrow(groups$A) + nrow(groups$B), nrow(pdf))
})

test_that("split_groups warns when no Group A country is present", {
  df  <- make_mini_panel()
  df  <- df[!df$country %in% c("Saudi", "UAE", "Bahrain", "Egypt"), ]
  pdf <- build_pdata(df)
  expect_warning(split_groups(pdf), "Group A")
})

test_that("load_panel errors clearly when the data file is missing", {
  expect_error(
    load_panel(path = "data/raw/__does_not_exist__.xlsx", sheet = "x"),
    "Data file not found"
  )
})

test_that("find_project_root locates the directory containing DESCRIPTION", {
  root <- find_project_root()
  expect_true(file.exists(file.path(root, "DESCRIPTION")))
})
