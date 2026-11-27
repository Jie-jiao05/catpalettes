#' Export a colour palette
#'
#' Save category names and their assigned colours to a CSV file.
#'
#' @param colours A character vector of valid colours.
#' @param file File path for the CSV. Defaults to "colour_mapping.csv".
#'
#' @return Invisibly returns a data frame with category and colour
#'   columns. A group column is included if group information
#'   is available.
#'
#' @details
#' If the colour vector is unnamed, category names are assigned
#' automatically.
#'
#' @seealso [select_palette()], [nest_palette()]
#' @export
#'
#' @examples
#' colours <- c(A = "red", B = "blue", C = "green")
#' file <- tempfile(fileext = ".csv")
#' export_palette(colours, file = file)
#' unlink(file)
export_palette <- function(colours, file = "colour_mapping.csv") {
  groups <- attr(colours, "groups")
  colours <- .check_colours(colours)
  categories <- names(colours)
  if (is.null(categories)) categories <- paste("Category", seq_along(colours))
  categories <- .check_categories(categories)
  result <- data.frame(
    category = categories, colour = unname(colours), stringsAsFactors = FALSE
  )
  if (!is.null(groups)) {
    if (is.null(names(groups)) || anyDuplicated(names(groups)) ||
        !all(categories %in% names(groups))) {
      stop("Stored group names do not match the categories.", call. = FALSE)
    }
    result$group <- unname(groups[categories])
  }
  utils::write.csv(result, file = file, row.names = FALSE)
  invisible(result)
}