# =============================================================================
# Panel transforms: first-differencing and per-variable cleaning
#
# Consolidates logic previously duplicated and side-effecting in
# Stationary nb 1/2/3.R and mODELS AFTER STATION.R.
#
# CRITICAL behaviour preserved exactly:
#   * d_ln_gdpc is the per-country first difference ln_gdpc - lag(ln_gdpc),
#     computed after arrange(country, year), giving one leading NA per country.
#   * Unlike the originals (which did `df <<-`-style global reassignment),
#     these functions RETURN a new data.frame and never mutate the caller's.
# =============================================================================

#' Add a per-country first difference column
#'
#' Computes `x_t - x_{t-1}` within each country after ordering by year, exactly
#' as the original scripts did. Produces one leading `NA` per country. Row count
#' is unchanged (the NA rows are kept; downstream code filters them).
#'
#' @param data    A data.frame with `country`, `year`, and `var`.
#' @param var     Name of the variable to difference (default "ln_gdpc").
#' @param new_col Name of the output column (default "d_<var>").
#' @return The input data.frame with one additional numeric column. Row order is
#'   country, year (matching the original arrange()).
#' @export
add_first_difference <- function(data, var = "ln_gdpc",
                                 new_col = paste0("d_", var)) {
  stopifnot(is.data.frame(data),
            all(c("country", "year", var) %in% names(data)))

  out <- data |>
    # Coerce year via character first: as.integer() on a factor/pseries would
    # return level codes, not the year value, and could mis-order the lag.
    dplyr::mutate(year = as.integer(as.character(.data$year))) |>
    dplyr::arrange(.data$country, .data$year) |>
    dplyr::group_by(.data$country) |>
    dplyr::mutate(!!new_col := .data[[var]] - dplyr::lag(.data[[var]])) |>
    dplyr::ungroup()

  as.data.frame(out)
}

#' Clean a single panel variable for unit-root testing
#'
#' Selects one variable, coerces types, drops missing/non-finite values and
#' duplicate (country, year) pairs, and keeps only countries with at least
#' `min_T` observations. Identical to the `clean_panel_var()` helper that was
#' duplicated across the stationarity scripts.
#'
#' @param data    A data.frame containing `country`, `year`, and `varname`.
#' @param varname Name of the variable to clean.
#' @param min_T   Minimum observations per country to retain (default 6).
#' @return A cleaned data.frame with an added numeric `value` column.
#' @export
clean_panel_var <- function(data, varname, min_T = 6) {
  stopifnot(is.data.frame(data),
            all(c("country", "year", varname) %in% names(data)))

  data |>
    dplyr::mutate(
      country = as.factor(.data$country),
      # via character: as.integer() on a factor/pseries returns level codes
      # (1, 2, 3, ...) rather than the actual year, silently corrupting time.
      year    = as.integer(as.character(.data$year)),
      value   = as.numeric(.data[[varname]])
    ) |>
    dplyr::filter(
      !is.na(.data$country),
      !is.na(.data$year),
      is.finite(.data$value)
    ) |>
    dplyr::arrange(.data$country, .data$year) |>
    dplyr::distinct(.data$country, .data$year, .keep_all = TRUE) |>
    dplyr::group_by(.data$country) |>
    dplyr::filter(dplyr::n() >= min_T) |>
    dplyr::ungroup() |>
    as.data.frame()
}
