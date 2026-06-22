# =============================================================================
# Plots (return ggplot objects; the caller decides whether to save)
# =============================================================================

#' Residuals-vs-fitted diagnostic plot
#'
#' Port of the heteroskedasticity diagnostic plot (Heteroscedasticity plot.R):
#' a scatter of residuals against fitted values with a dashed zero line.
#'
#' @param model A fitted model (lm or plm).
#' @param title Plot title.
#' @param x_lab,y_lab Axis labels.
#' @return A `ggplot` object.
#' @export
plot_resid_fitted <- function(model,
                              title = "Residuals vs Fitted",
                              x_lab = "Fitted values",
                              y_lab = "Residuals") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_resid_fitted() needs the 'ggplot2' package.", call. = FALSE)
  }
  d <- data.frame(Fitted = as.numeric(stats::fitted(model)),
                  Residuals = as.numeric(stats::residuals(model)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Fitted, y = .data$Residuals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) +
    ggplot2::theme_bw()
}

#' Heterogeneity plot: group means of ln_trade with 95% CI error bars
#'
#' ggplot equivalent of the legacy `gplots::plotmeans()` heterogeneity charts
#' (mean ln_trade by country or by year, with confidence bars), used to
#' visualise the unobserved heterogeneity that motivates panel models.
#'
#' @param data A data.frame.
#' @param by   Grouping variable: "country" or "year".
#' @return A `ggplot` object.
#' @export
plot_heterogeneity <- function(data, by = c("country", "year")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_heterogeneity() needs the 'ggplot2' package.", call. = FALSE)
  }
  by <- match.arg(by)
  d  <- as.data.frame(data)
  d[[by]] <- as.factor(as.character(d[[by]]))
  agg <- do.call(rbind, lapply(split(d, d[[by]]), function(g) {
    n <- sum(!is.na(g$ln_trade)); m <- mean(g$ln_trade, na.rm = TRUE)
    se <- stats::sd(g$ln_trade, na.rm = TRUE) / sqrt(n)
    data.frame(grp = g[[by]][1], mean = m,
               lo = m - 1.96 * se, hi = m + 1.96 * se)
  }))
  ggplot2::ggplot(agg, ggplot2::aes(x = .data$grp, y = .data$mean,
                                    group = 1)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                           width = 0.2, colour = "steelblue") +
    ggplot2::geom_point(colour = "steelblue", size = 2) +
    ggplot2::geom_line(colour = "black") +
    ggplot2::labs(title = paste("Heterogeneity across", by),
                  x = by, y = "Log of Trade") +
    ggplot2::theme_bw()
}

#' Scatter of a regressor against ln_trade, coloured by country
#'
#' Port of the scatter plots in the legacy descriptive script.
#'
#' @param data A data.frame.
#' @param xvar Name of the x-axis variable.
#' @param title Plot title.
#' @return A `ggplot` object.
#' @export
scatter_vs_trade <- function(data, xvar, title = paste("Trade vs", xvar)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("scatter_vs_trade() needs the 'ggplot2' package.", call. = FALSE)
  }
  d <- as.data.frame(data)
  d <- d[!is.na(d$country), ]
  ggplot2::ggplot(d, ggplot2::aes(x = .data[[xvar]], y = .data$ln_trade,
                                  color = .data$country)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = title, x = xvar, y = "Log of Trade", color = "Country")
}
