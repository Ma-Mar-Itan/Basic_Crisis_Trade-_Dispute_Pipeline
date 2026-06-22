# =============================================================================
# Robust variance-covariance estimators
#
# Named wrappers around the exact vcov calls used in the legacy scripts, so the
# choice of robust SE is explicit and consistent at every call site.
# =============================================================================

#' Driscoll–Kraay (SCC) covariance matrix
#'
#' Equivalent to `plm::vcovSCC(model, type = type)`. Used in the legacy scripts
#' for panel (FE/RE) models — robust to heteroskedasticity, serial and
#' cross-sectional correlation. The legacy default was "HC1" for FE and "HC0"
#' in a couple of RE scripts; the caller chooses.
#'
#' @param model A `plm` model.
#' @param type  Small-sample adjustment ("HC0", "HC1", ...). Default "HC1".
#' @return A covariance matrix.
#' @export
driscoll_kraay <- function(model, type = "HC1") {
  plm::vcovSCC(model, type = type)
}

#' White (HC) covariance matrix for OLS
#'
#' Equivalent to `sandwich::vcovHC(model, type = type)`, as used with the OLS
#' models in the legacy Group scripts.
#'
#' @param model An `lm` model.
#' @param type  HC type. Default "HC1".
#' @return A covariance matrix.
#' @export
white_hc <- function(model, type = "HC1") {
  sandwich::vcovHC(model, type = type)
}

#' Coefficient test with a robust covariance matrix
#'
#' Convenience wrapper for `lmtest::coeftest(model, vcov = vcov_fun(model))`,
#' reproducing the `coeftest(fe, vcovSCC(fe, type="HC1"))` pattern.
#'
#' @param model    A fitted model.
#' @param vcov_fun A function taking `model` and returning a covariance matrix
#'   (e.g. [driscoll_kraay] or [white_hc]).
#' @return A `coeftest` (matrix) object.
#' @export
robust_coeftest <- function(model, vcov_fun = driscoll_kraay) {
  lmtest::coeftest(model, vcov = vcov_fun(model))
}
