# =============================================================================
# Internal utilities (no external dependencies)
# =============================================================================

#' Null-coalescing operator
#'
#' Returns `a` unless it is `NULL`, in which case `b`.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Locate the project root
#'
#' Walks up from the current working directory until it finds a directory
#' containing a project marker (`DESCRIPTION` or a `.Rproj` file). This replaces
#' the `here` dependency with a small base-R implementation so the package has
#' no extra runtime requirement just to resolve paths.
#'
#' @param start Directory to begin searching from (default: working directory).
#' @return Absolute path to the project root.
#' @keywords internal
#' @noRd
find_project_root <- function(start = getwd()) {
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    has_desc  <- file.exists(file.path(dir, "DESCRIPTION"))
    has_rproj <- length(list.files(dir, pattern = "[.]Rproj$")) > 0
    if (has_desc || has_rproj) return(dir)

    parent <- dirname(dir)
    if (identical(parent, dir)) {
      # Reached filesystem root without finding a marker; fall back to start.
      return(normalizePath(start, winslash = "/", mustWork = FALSE))
    }
    dir <- parent
  }
}

#' Test whether a path is absolute (Windows or POSIX)
#' @keywords internal
#' @noRd
is_absolute_path <- function(p) {
  grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", p)
}

#' Build an absolute path relative to the project root
#'
#' Drop-in replacement for `here::here()` for this project's needs. If the
#' joined path is already absolute it is returned unchanged, so callers may
#' pass either a project-relative path (e.g. "config/config.yml") or a fully
#' qualified one.
#'
#' @param ... Path components, relative to the project root.
#' @return A single path string.
#' @keywords internal
#' @noRd
proj_path <- function(...) {
  joined <- do.call(file.path, as.list(c(...)))
  if (is_absolute_path(joined)) return(joined)
  file.path(find_project_root(), joined)
}
