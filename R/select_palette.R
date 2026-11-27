#' Select colours for categorical data
#'
#' Select colours from an existing paletteer palette or a custom
#' vector of colours and assign them to categories.
#'
#' @param palette A paletteer identifier in "package::palette"
#'   format.
#' @param categories An optional vector of unique category names.
#' @param n An optional positive integer specifying the number
#'   of colours to select.
#' @param colours An optional character vector of custom colours.
#'
#' @return A character vector of hexadecimal colours, named
#'   by category when supplied.
#'
#' @details
#' 
#' Supply either palette or colours, but not both.
#' Colours are selected in their original order without
#' interpolation. Transparency is preserved.
#' If categories and n are both supplied, their lengths
#' must agree.
#'
#' @seealso [find_palettes()], [nest_palette()], [plot_palette()]
#' @export
#'
#' @examples
#' select_palette(palette = "RColorBrewer::Set2", n = 3)
#'
#' select_palette(
#'   colours = c("red", "blue", "green"),
#'   categories = c("A", "B", "C")
#' )
select_palette <- function(palette = NULL, categories = NULL, n = NULL, colours = NULL) {
  if (is.null(palette) == is.null(colours)) {
    stop("Supply either palette or colours, but not both.", call. = FALSE)
  }
  if (!is.null(categories)) {
    categories <- .check_categories(categories)
    if (!is.null(n) && .check_n(n) != length(categories)) {
      stop("n must equal the number of categories.", call. = FALSE)
    }
    n <- length(categories)
  }
  if (!is.null(n)) n <- .check_n(n)

  if (!is.null(palette)) {
    if (!is.character(palette) || length(palette) != 1L || is.na(palette)) {
      stop("palette must be one paletteer identifier.", call. = FALSE)
    }
    available <- as.character(paletteer::paletteer_d(palette))
  } else {
    available <- colours
  }

  if (!is.character(available) || !length(available)) {
    stop("The palette must contain a non-empty character vector of colours.", call. = FALSE)
  }
  if (is.null(n)) n <- length(available)
  if (n > length(available)) {
    stop("Requested ", n, " colours; only ", length(available), " are available.", call. = FALSE)
  }
  selected <- .check_colours(available[seq_len(n)])
  if (!is.null(categories)) names(selected) <- categories
  selected
}
