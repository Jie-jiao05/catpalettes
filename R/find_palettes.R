#' Find palettes by size
#'
#' Find discrete palettes in paletteer that contain at least
#' the requested number of colours.
#'
#' @param n Number of colours needed.
#' @param type Optional palette type (e.g. "qualitative").
#'   Defaults to NULL, which includes all palette types.
#'
#' @return A data frame with the following columns:
#'   - palette_id: Palette identifier.
#'   - n_available: Number of available colours.
#'   - type: Palette type.
#'   - n_selected: Number of requested colours.
#'
#' @details
#' Results are sorted alphabetically by palette identifier.
#' Palettes are not screened for colour accessibility.
#'
#' @seealso [select_palette()], [assess_palette()], [plot_palettes()]
#' @export
#'
#' @examples
#' find_palettes(n = 5)
#'
#' find_palettes(n = 5, type = "qualitative")
find_palettes <- function(n, type = NULL) {
  n <- .check_n(n)
  catalogue <- as.data.frame(paletteer::palettes_d_names)
  if (!is.null(type) && (!is.character(type) || length(type) != 1L ||
      is.na(type) || !type %in% catalogue$type)) {
    stop("Use a catalogue type (e.g. 'qualitative') or NULL.", call. = FALSE)
  }
  keep <- catalogue$length >= n
  if (!is.null(type)) keep <- keep & catalogue$type == type
  catalogue <- catalogue[keep, , drop = FALSE]
  result <- data.frame(
    palette_id = paste(catalogue$package, catalogue$palette, sep = "::"),
    n_available = catalogue$length,
    type = as.character(catalogue$type),
    n_selected = rep(n, nrow(catalogue)),
    stringsAsFactors = FALSE
  )
  result <- result[order(result$palette_id), , drop = FALSE]
  rownames(result) <- NULL
  if (!nrow(result)) message("No discrete palettes contain enough colours.")
  result
}