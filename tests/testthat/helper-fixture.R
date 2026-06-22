# Shared synthetic panel for fast, offline tests.
# 6 countries x 10 years; includes all four Group A states plus two others.
# Deterministic (no RNG) so tests are reproducible without a seed.

make_mini_panel <- function() {
  countries <- c("Saudi", "UAE", "Bahrain", "Egypt", "USA", "Japan")
  years     <- 2010:2019
  grid <- expand.grid(country = countries, year = years,
                      stringsAsFactors = FALSE)
  grid <- grid[order(grid$country, grid$year), ]

  n <- nrow(grid)
  ci <- match(grid$country, countries)          # 1..6 per country
  ti <- grid$year - min(years)                   # 0..9 per row

  data.frame(
    country  = grid$country,
    year     = grid$year,
    ln_trade = 10 + 0.5 * ci + 0.10 * ti,
    ln_cti   =  2 + 0.2 * ci + 0.05 * ti,
    ln_gdpc  =  8 + 0.3 * ci + 0.08 * ti,
    distance = 100 * ci,                          # time-invariant on purpose
    crises   = as.numeric(grid$year >= 2017),     # blockade-era dummy
    stringsAsFactors = FALSE
  )
}
