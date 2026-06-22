# =============================================================================
# Descriptive statistics
# =============================================================================

#' Panel summary statistics for a fitted model
#'
#' Port of `panel_stats_plm()` from Final RE.R: mean and SD of the dependent
#' variable, residual sum of squares, and the regression standard error.
#'
#' @param model A fitted `plm` model.
#' @return A one-row data.frame.
#' @export
panel_stats <- function(model) {
  y <- model$model[[1]]
  u <- stats::residuals(model)
  data.frame(
    Mean_dep_var  = mean(y, na.rm = TRUE),
    SD_dep_var    = stats::sd(y, na.rm = TRUE),
    Sum_sq_resid  = sum(u^2),
    SE_regression = sqrt(sum(u^2) / stats::df.residual(model))
  )
}

#' Descriptive statistics by country
#'
#' Port of the `describeBy()` block in the legacy descriptive script. Requires
#' the `psych` package (Suggests). Returns the per-country/per-variable matrix.
#'
#' @param data A data.frame.
#' @param vars Character vector of variables to describe.
#' @return A data.frame of grouped descriptives (psych::describeBy, mat = TRUE).
#' @export
describe_by_country <- function(data,
                                vars = c("ln_trade", "ln_cti", "ln_gdpc",
                                         "distance", "crises")) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("describe_by_country() needs the 'psych' package.", call. = FALSE)
  }
  d <- as.data.frame(data)
  as.data.frame(psych::describeBy(d[, vars], group = d[["country"]], mat = TRUE))
}
