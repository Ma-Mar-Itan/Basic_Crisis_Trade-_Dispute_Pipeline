# =============================================================================
# Data loading & panel construction
#
# Single canonical implementation of the load + split logic that was previously
# copy-pasted across ~8 scripts (1_load and split data.R, Final RE.R,
# Ibrahim_FE.vs.RE.R, Alternative Random effects model.R, Husman Test ....R,
# the Group A/B files, etc.).
#
# Behaviour is preserved exactly:
#   * read sheet "data_for_model"
#   * coerce `year` and `crises` to numeric
#   * build a pdata.frame indexed on c("country", "year")
#   * Group A = c("Saudi","UAE","Bahrain","Egypt"); Group B = the complement
# =============================================================================

#' Read the project configuration
#'
#' @param path Path to the YAML config, relative to the project root.
#' @return A named list (the parsed `config/config.yml`).
#' @examples
#' \dontrun{ cfg <- load_config() }
#' @export
load_config <- function(path = "config/config.yml") {
  full <- proj_path(path)
  if (!file.exists(full)) {
    stop("Config file not found: ", full, call. = FALSE)
  }
  yaml::read_yaml(full)
}

#' Load the Qatar trade panel from Excel
#'
#' Reads the raw workbook, coerces `year` and `crises` to numeric (matching the
#' original scripts), validates the expected schema, and returns a plain
#' data.frame. Conversion to a panel is done separately by [build_pdata()] so
#' this step stays easy to test.
#'
#' @param path  Path to the .xlsx file, relative to the project root.
#' @param sheet Worksheet name.
#' @return A `data.frame` with the validated raw columns.
#' @export
load_panel <- function(path = NULL, sheet = NULL) {
  if (is.null(path) || is.null(sheet)) {
    cfg   <- load_config()
    path  <- path  %||% cfg$data$path
    sheet <- sheet %||% cfg$data$sheet
  }

  full <- proj_path(path)
  if (!file.exists(full)) {
    stop(
      "Data file not found: ", full,
      "\n  -> Place 'Qatar_Data_V.1.0.xlsx' in data/raw/ (see data/README.md).",
      call. = FALSE
    )
  }

  message("Loading data: ", full, " [sheet: ", sheet, "]")
  data <- readxl::read_excel(full, sheet = sheet)

  # --- schema validation FIRST (so a missing column gives a clear message
  #     instead of a cryptic coercion error on data$year / data$crises) ------
  required <- c("country", "year", "ln_trade", "ln_cti",
                "ln_gdpc", "distance", "crises")
  missing  <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Data is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # --- type coercion (preserves original behaviour) ------------------------
  data$year   <- as.numeric(data$year)
  data$crises <- as.numeric(data$crises)

  message("Loaded ", nrow(data), " rows, ",
          length(unique(data$country)), " countries.")
  as.data.frame(data)
}

#' Convert a raw data.frame to a plm panel
#'
#' @param data  A data.frame (typically from [load_panel()]).
#' @param index Length-2 character vector: the (id, time) columns.
#' @return A `pdata.frame` indexed on `index`.
#' @export
build_pdata <- function(data, index = c("country", "year")) {
  stopifnot(is.data.frame(data),
            length(index) == 2L,
            all(index %in% names(data)))
  plm::pdata.frame(data, index = index)
}

#' Split a panel into Group A (blockading states) and Group B (the rest)
#'
#' Group A is the four states that imposed the 2017 blockade on Qatar. Group B
#' is defined as the complement, exactly as in the original scripts.
#'
#' @param pdata          A `pdata.frame` (from [build_pdata()]).
#' @param group_a        Character vector of Group A country names.
#' @return A named list with elements `A` and `B`, each a `pdata.frame`.
#' @export
split_groups <- function(pdata,
                         group_a = c("Saudi", "UAE", "Bahrain", "Egypt")) {
  present <- intersect(group_a, as.character(pdata$country))
  if (length(present) == 0L) {
    warning("None of the Group A countries (",
            paste(group_a, collapse = ", "),
            ") are present in the data.", call. = FALSE)
  }

  is_a <- as.character(pdata$country) %in% group_a
  list(
    A = pdata[is_a, ],
    B = pdata[!is_a, ]
  )
}
