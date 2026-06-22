# =============================================================================
# Information criteria
#
# Single home for the criterion functions that were redefined in four legacy
# Group files (HQ()) and computed manually in LOG AIC BIC.R for the FE model.
# =============================================================================

#' Hannan–Quinn criterion
#'
#' Identical to the `HQ()` helper duplicated across the legacy Group scripts:
#' `-2*logLik + 2*k*log(log(n))`, with `k = attr(logLik, "df")` and
#' `n = nobs(model)`.
#'
#' @param model A fitted model with `logLik`/`nobs` methods (e.g. `lm`).
#' @return Numeric HQ value.
#' @export
hannan_quinn <- function(model) {
  ll <- as.numeric(stats::logLik(model))
  k  <- attr(stats::logLik(model), "df")
  n  <- stats::nobs(model)
  -2 * ll + 2 * k * log(log(n))
}

#' Standard information criteria
#'
#' Returns logLik, AIC, BIC and HQ using the base `logLik`/`AIC`/`BIC` methods,
#' matching the `AIC(ols)`, `BIC(ols)`, `HQ(ols)` calls in the legacy Group
#' scripts. Intended for `lm` (OLS) models.
#'
#' @param model A fitted `lm` model.
#' @return A one-row data.frame: logLik, AIC, BIC, HQ.
#' @export
info_criteria <- function(model) {
  data.frame(
    logLik = as.numeric(stats::logLik(model)),
    AIC    = stats::AIC(model),
    BIC    = stats::BIC(model),
    HQ     = hannan_quinn(model)
  )
}

#' Manual FE information criteria (residual-based)
#'
#' Reproduces LOG AIC BIC.R exactly: builds a Gaussian log-likelihood from the
#' within-model residuals (`sigma2 = mean(res^2)`), then AIC/BIC/HQC with
#' `k = length(coef(model))`.
#'
#' @param model A `plm` within (FE) model.
#' @return A list: logLik, AIC, BIC, HQC.
#' @export
fe_manual_criteria <- function(model) {
  res    <- stats::residuals(model)
  n      <- length(res)
  sigma2 <- mean(res^2)
  loglik <- -n / 2 * (log(2 * pi) + 1 + log(sigma2))
  k      <- length(stats::coef(model))
  list(
    logLik = loglik,
    AIC    = -2 * loglik + 2 * k,
    BIC    = -2 * loglik + k * log(n),
    HQC    = -2 * loglik + 2 * k * log(log(n))
  )
}
