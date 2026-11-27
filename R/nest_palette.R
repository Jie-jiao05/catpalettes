#' Create a nested colour palette
#'
#' Assign similar shades to categories within the same group.
#'
#' @param parent Optional vector of parent colours, named by group.
#' @param categories A vector of unique category names.
#' @param groups A vector of group names, one per category.
#' @param hues Optional numeric vector of group hues in degrees.
#' @param chroma Chroma of generated colours. Defaults to 45.
#' @param luminance Two numeric values specifying the luminance
#'   range. Defaults to c(35, 85).
#' @param max_delta_e Maximum colour distance between a parent
#'   colour and a matching palette shade.
#' @param max_hue_spread Maximum hue spread within a matching
#'   palette, ranging from 0 to 180 degrees.
#' @param sources Palette sources to search: "dynamic",
#'   "discrete", or both.
#' @param background A single opaque background colour.
#'
#' @return A named character vector of colours with two attributes:
#'   - groups: Group membership for each category.
#'   - report: A data frame describing how colours were selected
#'     for each group.
#'
#' @details
#' If parent is NULL, colours are generated in the HCL colour
#' space using supplied or automatically selected hues.
#'
#' If parent colours are supplied, the function searches for
#' matching sequential palettes. If no match is found, shades
#' are generated from the parent colour.
#'
#' The hues and chroma arguments can only be specified when
#' parent is NULL. The max_delta_e, max_hue_spread and sources
#' arguments can only be specified when parent colours are supplied.
#'
#' @seealso [select_palette()], [plot_palette()], [export_palette()]
#' @export
#'
#' @examples
#' # Without parent colours
#' nest_palette(
#'   categories = c("A1", "A2", "B1", "B2"),
#'   groups = c("A", "A", "B", "B")
#' )
#'
#' # With parent colours
#' \dontrun{
#' nest_palette(
#'   parent = c(A = "blue", B = "red"),
#'   categories = c("A1", "A2", "B1", "B2"),
#'   groups = c("A", "A", "B", "B")
#' )
#' }
nest_palette <- function(
    parent = NULL, categories, groups, hues = NULL, chroma = 45,
    luminance = c(35, 85), max_delta_e = 10, max_hue_spread = 20,
    sources = c("dynamic", "discrete"), background = "#FFFFFF") {
  categories <- .check_categories(categories)
  groups <- as.character(groups)
  if (length(groups) != length(categories) || anyNA(groups) ||
      any(!nzchar(trimws(groups)))) {
    stop("Supply one non-empty group name for each category.", call. = FALSE)
  }
  background <- .check_background(background)

  if (is.null(parent)) {
    if (!missing(max_delta_e) || !missing(max_hue_spread) || !missing(sources)) {
      stop("Matching settings require parent colours.", call. = FALSE)
    }
    result <- .generate_groups(categories, groups, hues, luminance, chroma)
    group_names <- unique(groups)
    # Report parent matching as not applicable for independently generated colours.
    attr(result, "report") <- data.frame(
      group = group_names, parent = NA_character_,
      n = vapply(group_names, function(g) sum(groups == g), integer(1)),
      hue = NA_real_, chroma = chroma, luminance = NA_real_,
      method = "generated grouped HCL colours",
      palette_id = NA_character_, delta_e = NA_real_,
      row.names = NULL, stringsAsFactors = FALSE
    )
    return(result)
  }

  if (!is.null(hues) || !missing(chroma)) {
    stop("Use hues and chroma only when parent is omitted.", call. = FALSE)
  }
  if (is.null(names(parent)) || anyNA(names(parent)) ||
      any(!nzchar(trimws(names(parent)))) || anyDuplicated(names(parent))) {
    stop("parent must have unique, non-empty group names.", call. = FALSE)
  }
  max_hue_spread <- .check_threshold(max_hue_spread)
  if (max_hue_spread > 180) {
    stop("max_hue_spread must be between 0 and 180 degrees.", call. = FALSE)
  }
  .nest_from_parent(
    parent = parent, categories = categories, groups = groups,
    max_delta_e = max_delta_e, max_hue_spread = max_hue_spread,
    luminance = luminance, sources = sources, background = background
  )
}
