#' Composite transparent colours onto a background
#'
#' Combines transparent colours with an opaque background
#' to determine their displayed appearance.
#'
#' @param colours A character vector of valid colours.
#' @param background A single opaque background colour.
#' @return A character vector of opaque hexadecimal colours.
#' @noRd
.flatten_colours <- function(colours, background = "#FFFFFF") {
  colours <- .check_colours(colours)
  background <- .check_background(background)
  foreground <- grDevices::col2rgb(colours, alpha = TRUE)
  backdrop <- grDevices::col2rgb(background)
  alpha <- foreground[4, ] / 255
  rgb <- sweep(foreground[1:3, , drop = FALSE], 2, alpha, "*") +
    outer(backdrop[, 1], 1 - alpha)
  result <- grDevices::rgb(
    rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255
  )
  stats::setNames(result, names(colours))
}


#' Transform colours for different display conditions
#'
#' Simulates the appearance of colours under grayscale
#' and different colour vision deficiency conditions.
#'
#' @param colours A character vector of valid colours.
#' @param view The display condition to simulate. Options include
#'   "original", "grayscale", "deutan", "protan" and "tritan".
#' @param severity A numeric value between 0 and 1 specifying
#'   the severity of colour vision deficiency.
#' @param background A single opaque background colour.
#' @return A character vector of transformed colours.
#' @noRd
.transform_colours <- function(
    colours, view = "original", severity = 1, background = "#FFFFFF") {
  view <- .check_views(view)
  if (length(view) != 1L) stop("Supply one view.", call. = FALSE)
  severity <- .check_severity(severity)
  colours <- .flatten_colours(colours, background)
  result <- switch(
    view,
    original = colours,
    grayscale = colorspace::desaturate(colours),
    deutan = colorspace::deutan(colours, severity = severity),
    protan = colorspace::protan(colours, severity = severity),
    tritan = colorspace::tritan(colours, severity = severity)
  )
  stats::setNames(as.character(result), names(colours))
}