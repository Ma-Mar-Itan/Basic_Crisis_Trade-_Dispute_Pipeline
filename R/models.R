# =============================================================================
# Model estimation
#
# Thin, faithful wrappers around the exact lm()/plm() calls used in the legacy
# scripts (Group A/B files, Final RE.R, Alternative Random effects model.R,
# old_code Chamberlain-Mundlak). Each takes data + a formula and returns the
# fitted model object unchanged, so downstream summary()/coeftest()/AIC()
# behave identically to the originals.
# =============================================================================

# The canonical gravity specification (see config/config.yml::model$formula).
default_formula <- function() {
  ln_trade ~ ln_cti + ln_gdpc + distance + crises
}

#' Pooled OLS
#'
#' Equivalent to `lm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data)`.
#' @param data    A data.frame / pdata.frame.
#' @param formula Model formula (defaults to the gravity specification).
#' @return An `lm` object.
#' @export
fit_ols <- function(data, formula = default_formula()) {
  stats::lm(formula, data = data)
}

#' Fixed-effects (within) estimator
#'
#' Equivalent to `plm(formula, data, index = c("country","year"),
#' model = "within")`.
#' @inheritParams fit_ols
#' @param index Panel index (id, time).
#' @return A `plm` object.
#' @export
fit_fe <- function(data, formula = default_formula(),
                   index = c("country", "year")) {
  plm::plm(formula, data = data, index = index, model = "within")
}

#' Random-effects estimator
#'
#' Equivalent to `plm(formula, data, model = "random",
#' random.method = method)`. The legacy scripts use "walhus" as the headline
#' method; "amemiya" and "nerlove" are also supported.
#' @inheritParams fit_fe
#' @param method GLS variance-component method: "walhus", "amemiya", "nerlove".
#' @return A `plm` object.
#' @export
fit_re <- function(data, formula = default_formula(),
                   method = c("walhus", "amemiya", "nerlove"),
                   index = c("country", "year")) {
  method <- match.arg(method)
  plm::plm(formula, data = data, index = index,
           model = "random", random.method = method)
}

#' Chamberlain–Mundlak Correlated Random Effects (CRE) model
#'
#' Adds country-level means of each regressor (via `ave(x, country)`) and fits a
#' random-effects model, exactly as in the legacy CRE script. Time-invariant
#' regressors (e.g. `distance`) have a constant group mean by construction.
#'
#' @param data       A data.frame / pdata.frame with `country` and the regressors.
#' @param response   Name of the dependent variable (default "ln_trade").
#' @param regressors Character vector of regressor names.
#' @param index      Panel index (id, time).
#' @return A `plm` random-effects object including the group-mean terms.
#' @export
fit_cre <- function(data,
                    response   = "ln_trade",
                    regressors = c("ln_cti", "ln_gdpc", "distance", "crises"),
                    index      = c("country", "year")) {
  d <- data
  mean_names <- paste0("mean_", regressors)
  for (i in seq_along(regressors)) {
    d[[mean_names[i]]] <- stats::ave(as.numeric(d[[regressors[i]]]),
                                     d[["country"]])
  }
  rhs <- paste(c(regressors, mean_names), collapse = " + ")
  form <- stats::as.formula(paste(response, "~", rhs))
  plm::plm(form, data = d, index = index, model = "random")
}
