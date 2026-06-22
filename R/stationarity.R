# =============================================================================
# Panel unit-root & stationarity testing
#
# Consolidates the four legacy stationarity scripts (Panel root.R,
# Stationary nb 1/2/3.R). The panel-test engine ports Panel root.R's
# defensive run_panel_test() verbatim (cleaning, purtest, and the
# "htest lives inside $statistic" extraction). run_country_adf() ports the
# per-country ADF approach from Stationary nb 2.R.
# =============================================================================

# Human-readable labels for the supported purtest codes.
.unit_root_labels <- c(
  ips = "Im-Pesaran-Shin", madwu = "Maddala-Wu", Pm = "Choi Modified P",
  invnormal = "Choi Inverse Normal", logit = "Choi Logit", hadri = "Hadri"
)

#' Run one panel unit-root test for one variable
#'
#' Faithful port of Panel root.R::run_panel_test(): cleans the variable, keeps
#' countries with enough observations and variation, runs `plm::purtest`, and
#' extracts the statistic / p-value from the nested htest. Returns a one-row
#' tibble (including an explicit failure row on error), never throws.
#'
#' @param data          A data.frame with country, year and `variable_name`.
#' @param variable_name Variable to test.
#' @param test_name     purtest code: ips, madwu, Pm, invnormal, logit, hadri.
#' @param exo_type      "intercept" or "trend".
#' @param lag_value     Lag order.
#' @return A one-row tibble of results.
#' @export
run_unit_root_test <- function(data, variable_name, test_name,
                               exo_type = "intercept", lag_value = 1) {
  is_hadri <- identical(test_name, "hadri")
  null_hyp <- if (is_hadri) "Stationarity" else "Unit root"
  fail_row <- function(countries, obs, concl, err) {
    tibble::tibble(
      variable = variable_name, test_code = test_name,
      test = unname(.unit_root_labels[test_name]), specification = exo_type,
      lags = lag_value, countries = countries, observations = obs,
      statistic_name = NA_character_, statistic = NA_real_,
      degrees_freedom = NA_character_, p_value = NA_real_,
      null_hypothesis = null_hyp, conclusion_5pct = concl, error = err
    )
  }

  test_data <- data |>
    dplyr::select(country, year, dplyr::all_of(variable_name)) |>
    dplyr::filter(!is.na(country), !is.na(year),
                  !is.na(.data[[variable_name]]),
                  is.finite(.data[[variable_name]])) |>
    dplyr::arrange(country, year) |>
    dplyr::distinct(country, year, .keep_all = TRUE)

  valid <- test_data |>
    dplyr::group_by(country) |>
    dplyr::summarise(n_obs = dplyr::n(),
                     n_unique = dplyr::n_distinct(.data[[variable_name]], na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(n_obs >= lag_value + 5, n_unique > 1) |>
    dplyr::pull(country)
  test_data <- dplyr::filter(test_data, country %in% valid)

  n_countries <- dplyr::n_distinct(test_data$country)
  n_obs <- nrow(test_data)
  if (n_countries < 2) {
    return(fail_row(n_countries, n_obs, "Test not estimated",
                    "Fewer than two valid countries"))
  }

  pdf <- plm::pdata.frame(test_data, index = c("country", "year"),
                          drop.index = FALSE, row.names = TRUE)
  res <- tryCatch(
    plm::purtest(pdf[[variable_name]], test = test_name,
                 exo = exo_type, lags = lag_value),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(fail_row(n_countries, n_obs, "Test failed", conditionMessage(res)))
  }

  h <- res$statistic                      # purtest stores the htest here
  stat <- as.numeric(h$statistic[1])
  stat_name <- names(h$statistic)[1]
  if (is.null(stat_name) || length(stat_name) == 0 || is.na(stat_name)) {
    stat_name <- "Statistic"
  }
  p <- as.numeric(h$p.value[1])
  dfree <- if (!is.null(h$parameter) && length(h$parameter) > 0) {
    pn <- names(h$parameter); if (is.null(pn)) pn <- rep("parameter", length(h$parameter))
    paste(pn, as.numeric(h$parameter), sep = " = ", collapse = "; ")
  } else NA_character_

  concl <- if (is_hadri) {
    if (p < 0.05) "Reject stationarity; evidence that at least one panel series has a unit root"
    else "Fail to reject stationarity"
  } else {
    if (p < 0.05) "Reject unit root; evidence of stationarity"
    else "Fail to reject unit root"
  }

  tibble::tibble(
    variable = variable_name, test_code = test_name,
    test = unname(.unit_root_labels[test_name]), specification = exo_type,
    lags = lag_value, countries = n_countries, observations = n_obs,
    statistic_name = stat_name, statistic = stat, degrees_freedom = dfree,
    p_value = p, null_hypothesis = null_hyp, conclusion_5pct = concl,
    error = NA_character_
  )
}

#' Run a battery of panel unit-root tests
#'
#' Loops [run_unit_root_test()] over variables x tests and binds the rows,
#' reproducing the main loop of Panel root.R.
#'
#' @param data      A data.frame.
#' @param variables Character vector of variables.
#' @param tests     Character vector of purtest codes.
#' @param exo_type  "intercept" or "trend".
#' @param lag_value Lag order.
#' @return A tibble: one row per (variable, test).
#' @export
run_panel_unit_root <- function(data,
                                variables = c("ln_trade", "ln_cti", "ln_gdpc"),
                                tests = c("ips", "madwu", "Pm", "invnormal", "logit", "hadri"),
                                exo_type = "intercept", lag_value = 1) {
  rows <- list(); i <- 1
  for (v in variables) for (tt in tests) {
    rows[[i]] <- run_unit_root_test(data, v, tt, exo_type, lag_value); i <- i + 1
  }
  dplyr::bind_rows(rows)
}

#' Per-country Augmented Dickey-Fuller test
#'
#' Port of Stationary nb 2.R: cleans each variable (>= `min_T` obs per country)
#' and runs `tseries::adf.test` per country.
#'
#' @param data      A data.frame.
#' @param variables Character vector of variables.
#' @param lag_order ADF lag order `k` (default 0).
#' @param min_T     Minimum observations per country.
#' @return A tibble: country x variable ADF results.
#' @export
run_country_adf <- function(data, variables = c("ln_trade", "ln_cti", "ln_gdpc"),
                            lag_order = 0, min_T = 6) {
  dplyr::bind_rows(lapply(variables, function(v) {
    clean_panel_var(data, v, min_T = min_T) |>
      dplyr::group_by(country) |>
      dplyr::summarise(
        variable = v, n_obs = dplyr::n(),
        statistic = tryCatch(as.numeric(tseries::adf.test(value, k = lag_order)$statistic),
                             error = function(e) NA_real_),
        p_value = tryCatch(as.numeric(tseries::adf.test(value, k = lag_order)$p.value),
                           error = function(e) NA_real_),
        decision_5pct = ifelse(!is.na(p_value) & p_value < 0.05, "Stationary", "Non-stationary"),
        .groups = "drop"
      )
  }))
}
