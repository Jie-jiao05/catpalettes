#' Assess colour separation within a palette
#'
#' Calculates the minimum colour distance within a palette
#' under different colour vision conditions.
#'
#' @param colours A character vector of valid colours.
#' @param views A character vector of display conditions.
#'   Options are "original", "grayscale", "deutan",
#'   "protan" and "tritan".
#' @param threshold A finite non-negative numeric value
#'   specifying the minimum acceptable Delta E distance.
#' @param severity A numeric value between 0 and 1 specifying
#'   the severity of simulated colour vision deficiency.
#' @param background A single opaque background colour.
#'
#' @return A data frame containing one row per display condition,
#'   with minimum Delta E, closest colour pair, threshold,
#'   threshold status, severity and background.
#'
#' @details
#' Colour separation is calculated using the minimum pairwise
#' Euclidean distance in CIELAB colour space (Delta E 1976).
#' Transparent colours are composited onto the background.
#' Palettes containing only one colour return missing values
#' for pairwise separation measures.
#'
#' The threshold is a screening heuristic and does not
#' guarantee colour accessibility.
#'
#' @seealso [plot_palette()], [plot_palettes()]
#' @export
#'
#' @examples
#' assess_palette(c("red", "blue", "green"))
#'
#' assess_palette(
#'   colours = c("#D0006F", "#8B84D7"),
#'   views = c("original", "deutan"),
#'   threshold = 15
#' )
assess_palette <- function(
    colours, views = c("original", "deutan", "protan", "tritan", "grayscale"),
    threshold = 10, severity = 1, background = "#FFFFFF") {
  colours <- .check_colours(colours)
  views <- .check_views(views)
  threshold <- .check_threshold(threshold)
  severity <- .check_severity(severity)
  background <- .check_background(background)
  labels <- names(colours)
  if (is.null(labels)) labels <- paste0("Colour ", seq_along(colours))

  results <- lapply(views, function(view) {
    transformed <- .transform_colours(colours, view, severity, background)
    if (length(transformed) < 2L) {
      return(data.frame(
        view = view, min_delta_e = NA_real_, closest_pair = NA_character_,
        threshold = threshold, meets_threshold = NA,
        severity = severity, background = background, stringsAsFactors = FALSE
      ))
    }
    rgb <- t(grDevices::col2rgb(transformed)) / 255
    lab <- grDevices::convertColor(rgb, from = "sRGB", to = "Lab", scale.in = 1)
    distances <- as.matrix(stats::dist(lab))
    distances[!upper.tri(distances)] <- Inf
    pair <- arrayInd(which.min(distances), dim(distances))
    minimum <- distances[pair[1, 1], pair[1, 2]]

    data.frame(
      view = view, min_delta_e = minimum,
      closest_pair = paste(labels[pair[1, 1]], labels[pair[1, 2]], sep = " / "),
      threshold = threshold, meets_threshold = minimum >= threshold,
      severity = severity, background = background, stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, results)
  rownames(result) <- NULL
  result
}
