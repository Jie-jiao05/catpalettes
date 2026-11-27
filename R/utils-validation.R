#' Validate the number of colours
#'
#' @param n Number of colours requested.
#' @return A validated positive integer.
#' @noRd
.check_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      !is.finite(n) || n < 1 || n != floor(n) || n > .Machine$integer.max) {
    stop("n must be a positive whole number within R's integer range.", call. = FALSE)
  }
  as.integer(n)
}


#' Validate colour vision deficiency simulation severity
#'
#' @param severity A numeric value between 0 and 1.
#' @return The validated severity value.
#' @noRd
.check_severity <- function(severity) {
  if (!is.numeric(severity) || length(severity) != 1L || is.na(severity) ||
      !is.finite(severity) || severity < 0 || severity > 1) {
    stop("severity must be between 0 and 1.", call. = FALSE)
  }
  severity
}


#' Validate the colour separation threshold
#'
#' @param threshold A finite non-negative numeric value.
#' @return The validated threshold value.
#' @noRd
.check_threshold <- function(threshold) {
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      !is.finite(threshold) || threshold < 0) {
    stop("threshold must be a finite non-negative number.", call. = FALSE)
  }
  threshold
}


#' Validate colour display conditions
#'
#' @param views A character vector of display conditions.
#' @return A character vector of validated display conditions.
#' @noRd
.check_views <- function(views) {
  allowed <- c("original", "grayscale", "deutan", "protan", "tritan")
  if (!is.character(views) || !length(views) || anyNA(views) ||
      any(!views %in% allowed)) {
    stop("views must contain: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  unique(views)
}


#' Validate category names
#'
#' @param categories A vector of category names.
#' @return A validated character vector of category names.
#' @noRd
.check_categories <- function(categories) {
  categories <- as.character(categories)
  if (!length(categories) || anyNA(categories) ||
      any(!nzchar(trimws(categories))) || anyDuplicated(categories)) {
    stop("Category names must be non-empty, unique, and non-missing.", call. = FALSE)
  }
  categories
}


#' Validate and standardize colours
#'
#' @param colours A character vector of colours.
#' @return A character vector of hexadecimal colours,
#'   including alpha values.
#' @noRd
.check_colours <- function(colours) {
  if (!is.character(colours) || !length(colours) || anyNA(colours)) {
    stop("Supply a non-empty character vector of colours.", call. = FALSE)
  }
  rgba <- tryCatch(
    grDevices::col2rgb(colours, alpha = TRUE),
    error = function(e) stop("One or more colours are invalid.", call. = FALSE)
  )
  result <- grDevices::rgb(
    rgba[1, ], rgba[2, ], rgba[3, ],
    alpha = rgba[4, ], maxColorValue = 255
  )
  stats::setNames(result, names(colours))
}


#' Validate the background colour
#'
#' @param background A single opaque background colour.
#' @return A validated hexadecimal background colour.
#' @noRd
.check_background <- function(background) {
  if (!is.character(background) || length(background) != 1L || is.na(background)) {
    stop("background must be one opaque colour.", call. = FALSE)
  }
  background <- .check_colours(background)
  if (grDevices::col2rgb(background, alpha = TRUE)[4, 1] != 255L) {
    stop("background must be opaque.", call. = FALSE)
  }
  unname(background)
}