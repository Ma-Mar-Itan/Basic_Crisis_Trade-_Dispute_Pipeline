# =============================================================================
# Diagnostic tests
#
# Named wrappers around the exact diagnostic calls used in the legacy scripts:
# Hausman (phtest), Breusch-Pagan (bptest), Wooldridge (pwartest),
# Breusch-Godfrey (bgtest / pbgtest), Durbin-Watson (dwtest), Pesaran CD
# (pcdtest), and VIF. Each takes fitted model(s) and returns the test object,
# so printed output matches the originals.
# =============================================================================

#' Hausman test (FE vs RE)
#'
#' Equivalent to `plm::phtest(fe, re)`. Guards against the legacy bug of
#' comparing models fitted on different groups by checking the sample sizes
#' match.
#'
#' @param fe A fixed-effects `plm` model.
#' @param re A random-effects `plm` model (same data & regressors).
#' @return An `htest` object.
#' @export
hausman <- function(fe, re) {
  ids <- function(m) sort(unique(as.character(plm::index(m, "id"))))
  if (!identical(ids(fe), ids(re))) {
    stop("hausman(): fe and re are fitted on different individuals ",
         "(cross-group comparison). They must use the same group.\n",
         "  fe: ", paste(ids(fe), collapse = ", "), "\n",
         "  re: ", paste(ids(re), collapse = ", "), call. = FALSE)
  }
  plm::phtest(fe, re)
}

#' Breusch-Pagan test for heteroskedasticity
#' @param model An `lm` or formula-compatible model. @return An `htest`.
#' @export
bp_test <- function(model) lmtest::bptest(model)

#' Wooldridge test for serial correlation in panels (`pwartest`)
#' @param model A `plm` model. @return An `htest`.
#' @export
wooldridge_serial <- function(model) plm::pwartest(model)

#' Breusch-Godfrey test (`bgtest`) for OLS serial correlation
#' @param model An `lm` model. @param order Lag order. @return An `htest`.
#' @export
bg_test <- function(model, order = 1) lmtest::bgtest(model, order = order)

#' Durbin-Watson test (`dwtest`)
#' @param model An `lm` model. @return An `htest`.
#' @export
durbin_watson <- function(model) lmtest::dwtest(model)

#' Pesaran cross-sectional dependence test (`pcdtest`)
#' @param model A `plm` model.
#' @param test  "cd" (Pesaran CD) or "lm" (cross-correlation LM).
#' @return An `htest`.
#' @export
pesaran_cd <- function(model, test = c("cd", "lm")) {
  test <- match.arg(test)
  plm::pcdtest(model, test = test)
}

#' Strict-exogeneity test via lead regressors
#'
#' Port of the legacy strict-exogeneity check: add one-period leads of the
#' regressors and refit FE; joint significance of the leads is evidence against
#' strict exogeneity. Time-invariant regressors (distance) are excluded as their
#' leads are collinear within FE.
#'
#' @param data       A data.frame with country, year and the regressors.
#' @param response   Dependent variable name.
#' @param regressors Time-varying regressors to lead.
#' @param index      Panel index.
#' @return A `plm` within model including the lead terms (inspect via summary()).
#' @export
strict_exogeneity_test <- function(data,
                                   response = "ln_trade",
                                   regressors = c("ln_cti", "ln_gdpc", "crises"),
                                   index = c("country", "year")) {
  d <- as.data.frame(data)
  d <- d[order(d[["country"]], d[["year"]]), ]
  lead_names <- paste0("lead_", regressors)
  for (i in seq_along(regressors)) {
    d[[lead_names[i]]] <- stats::ave(
      as.numeric(d[[regressors[i]]]), d[["country"]],
      FUN = function(x) c(x[-1], NA)
    )
  }
  rhs  <- paste(c(regressors, lead_names), collapse = " + ")
  form <- stats::as.formula(paste(response, "~", rhs))
  plm::plm(form, data = d, index = index, model = "within")
}

#' Variance inflation factors (base-R; matches `car::vif` for lm without factors)
#'
#' For each regressor, `VIF_j = 1 / (1 - R^2_j)` where `R^2_j` comes from
#' regressing that regressor on all the others. Implemented without `car` to
#' keep the dependency surface small.
#'
#' @param model An `lm` model.
#' @return A named numeric vector of VIFs (one per non-intercept term).
#' @export
vif_values <- function(model) {
  X <- stats::model.matrix(model)
  keep <- colnames(X) != "(Intercept)"
  X <- X[, keep, drop = FALSE]
  preds <- colnames(X)
  if (length(preds) < 2) {
    stop("vif_values(): need at least two predictors.", call. = FALSE)
  }
  vapply(preds, function(p) {
    others <- setdiff(preds, p)
    df <- as.data.frame(X)
    fit <- stats::lm(
      stats::as.formula(paste0("`", p, "` ~ ",
                               paste(sprintf("`%s`", others), collapse = " + "))),
      data = df
    )
    1 / (1 - summary(fit)$r.squared)
  }, numeric(1))
}
