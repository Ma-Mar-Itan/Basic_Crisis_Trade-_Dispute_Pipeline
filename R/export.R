# =============================================================================
# Output helpers (the only module that writes files)
#
# Centralizes all artifact I/O to output/ via project-relative paths, replacing
# the scattered write.csv / write_xlsx / ggsave / save_as_docx calls.
# =============================================================================

#' Ensure an output directory exists and return its absolute path
#' @keywords internal
#' @noRd
ensure_dir <- function(dir) {
  full <- proj_path(dir)
  if (!dir.exists(full)) dir.create(full, recursive = TRUE, showWarnings = FALSE)
  full
}

#' Save a data.frame as CSV under output/tables
#' @param x A data.frame. @param filename File name (e.g. "results.csv").
#' @param dir Output directory (default "output/tables").
#' @return The path written (invisibly).
#' @export
save_table_csv <- function(x, filename, dir = "output/tables") {
  path <- file.path(ensure_dir(dir), filename)
  utils::write.csv(x, path, row.names = FALSE)
  invisible(path)
}

#' Save a data.frame as XLSX under output/tables
#' @inheritParams save_table_csv
#' @return The path written (invisibly).
#' @export
save_table_xlsx <- function(x, filename, dir = "output/tables") {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("save_table_xlsx() needs the 'writexl' package.", call. = FALSE)
  }
  path <- file.path(ensure_dir(dir), filename)
  writexl::write_xlsx(x, path)
  invisible(path)
}

#' Save a data.frame as a Word table under output/tables
#'
#' Port of the legacy Saveastable.R flextable/officer export.
#' @inheritParams save_table_csv
#' @param caption Optional table caption.
#' @return The path written (invisibly).
#' @export
save_table_docx <- function(x, filename, dir = "output/tables", caption = NULL) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("save_table_docx() needs the 'flextable' package.", call. = FALSE)
  }
  path <- file.path(ensure_dir(dir), filename)
  ft <- flextable::autofit(flextable::theme_booktabs(flextable::flextable(x)))
  if (!is.null(caption)) ft <- flextable::set_caption(ft, caption = caption)
  flextable::save_as_docx(ft, path = path)
  invisible(path)
}

#' Save a ggplot as SVG under output/figures
#' @param plot A ggplot object. @param filename File name (e.g. "resid.svg").
#' @param width,height Dimensions in inches. @param dir Output directory.
#' @return The path written (invisibly).
#' @export
save_plot_svg <- function(plot, filename, width = 7, height = 5,
                          dir = "output/figures") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("save_plot_svg() needs the 'ggplot2' package.", call. = FALSE)
  }
  path <- file.path(ensure_dir(dir), filename)
  ggplot2::ggsave(path, plot = plot, width = width, height = height,
                  device = "svg")
  invisible(path)
}
