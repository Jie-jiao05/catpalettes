#' Generate grouped colours
#'
#' Generate related colour shades for categories belonging
#' to the same group using the HCL colour space.
#'
#' @param categories A vector of unique category names.
#' @param groups A vector specifying group membership.
#' @param hues Optional numeric vector of group hues.
#' @param luminance Two numeric values specifying the
#'   luminance range.
#' @param chroma Numeric value specifying colour chroma.
#' @return A named character vector of grouped colours.
#' @noRd
.generate_groups <- function(
    categories, groups, hues = NULL, luminance = c(45, 80), chroma = 45) {
  categories <- .check_categories(categories)
  groups <- as.character(groups)
  if (length(groups) != length(categories) || anyNA(groups) ||
      any(!nzchar(trimws(groups)))) {
    stop("Supply one non-empty group name for each category.", call. = FALSE)
  }
  if (!is.numeric(luminance) || length(luminance) != 2L || anyNA(luminance) ||
      any(!is.finite(luminance)) || any(luminance < 0 | luminance > 100) ||
      luminance[1] > luminance[2]) {
    stop("luminance must be two ordered values between 0 and 100.")
  }
  if (!is.numeric(chroma) || length(chroma) != 1L || is.na(chroma) ||
      !is.finite(chroma) || chroma < 0) {
    stop("chroma must be a finite non-negative number.")
  }

  group_names <- sort(unique(groups), method = "radix")
  if (is.null(hues)) {
    positions <- seq(0, 360, length.out = length(group_names) + 1L)
    hues <- stats::setNames(
      (15 + positions[seq_along(group_names)]) %% 360, group_names
    )
  } else {
    if (!is.numeric(hues) || is.null(names(hues)) || anyDuplicated(names(hues)) ||
        !all(group_names %in% names(hues)) || anyNA(hues) || any(!is.finite(hues))) {
      stop("hues must be a named numeric vector covering every group.")
    }
    hues <- hues[group_names] %% 360
  }

  result <- stats::setNames(rep(NA_character_, length(categories)), categories)
  for (group in group_names) {
    members <- sort(categories[groups == group], method = "radix")
    lightness <- if (length(members) == 1L) {
      mean(luminance)
    } else {
      seq(luminance[1], luminance[2], length.out = length(members))
    }
    result[members] <- grDevices::hcl(
      h = hues[[group]], c = chroma, l = lightness, fixup = TRUE
    )
  }
  attr(result, "groups") <- stats::setNames(groups, categories)
  result
}


#' Convert colours to HCL coordinates
#'
#' @param colours A character vector of valid colours.
#' @return A matrix of hue, chroma and luminance coordinates.
#' @noRd
.hcl_coords <- function(colours) {
  colourspace <- methods::as(colorspace::hex2RGB(colours), "polarLUV")
  coords <- colorspace::coords(colourspace)
  coords[is.na(coords[, "H"]), "H"] <- 0
  coords
}


#' Convert colours to CIELAB coordinates
#'
#' @param colours A character vector of valid colours.
#' @return A matrix of CIELAB colour coordinates.
#' @noRd
.lab_coords <- function(colours) {
  rgb <- t(grDevices::col2rgb(colours)) / 255
  grDevices::convertColor(rgb, from = "sRGB", to = "Lab", scale.in = 1)
}


#' Calculate circular hue differences
#'
#' @param a Numeric hue values in degrees.
#' @param b Numeric hue values in degrees.
#' @return Numeric values representing the shortest circular
#'   differences between hues in degrees.
#' @noRd
.hue_difference <- function(a, b) {
  d <- abs(a - b) %% 360
  pmin(d, 360 - d)
}


# Internal cache for sequential palette catalogues
.catpalettes_cache <- new.env(parent = emptyenv())


#' Build a catalogue of sequential colour palettes
#'
#' Retrieve sequential palettes from paletteer and cache
#' the results to avoid repeated computations.
#'
#' @param sources A character vector specifying the palette
#'   sources: "dynamic", "discrete", or both.
#' @param background A single opaque background colour.
#' @return A list containing sequential palette information.
#' @noRd
.sequential_catalogue <- function(sources, background) {
  key <- paste(c(sort(sources), background), collapse = "|")
  if (!is.null(.catpalettes_cache[[key]])) return(.catpalettes_cache[[key]])

  listing <- list()
  if ("dynamic" %in% sources) {
    d <- as.data.frame(paletteer::palettes_dynamic_names)
    d <- d[d$type == "sequential", ]
    listing$dynamic <- data.frame(
      palette_id = paste(d$package, d$palette, sep = "::"),
      length = d$length, source = "dynamic", stringsAsFactors = FALSE
    )
  }
  if ("discrete" %in% sources) {
    d <- as.data.frame(paletteer::palettes_d_names)
    d <- d[d$type == "sequential", ]
    listing$discrete <- data.frame(
      palette_id = paste(d$package, d$palette, sep = "::"),
      length = d$length, source = "discrete", stringsAsFactors = FALSE
    )
  }
  listing <- do.call(rbind, listing)

  catalogue <- lapply(seq_len(nrow(listing)), function(i) {
    tryCatch({
      id <- listing$palette_id[i]
      colours <- if (listing$source[i] == "dynamic") {
        paletteer::paletteer_dynamic(id, listing$length[i])
      } else {
        paletteer::paletteer_d(id)
      }
      colours <- unname(.flatten_colours(as.character(colours), background))
      hcl <- .hcl_coords(colours)
      # Keep coloured shades only: drop near-white, near-black and grey ends.
      usable <- hcl[, "L"] >= 15 & hcl[, "L"] <= 92 & hcl[, "C"] >= 15
      if (sum(usable) < 2L) return(NULL)
      colours <- colours[usable]
      hcl <- hcl[usable, , drop = FALSE]
      radians <- hcl[, "H"] * pi / 180
      weights <- hcl[, "C"]
      mean_hue <- (atan2(sum(weights * sin(radians)),
                         sum(weights * cos(radians))) * 180 / pi) %% 360
      ordered <- order(hcl[, "L"])                     # dark to light
      list(
        palette_id = id, source = listing$source[i],
        colours = colours[ordered],
        lab = .lab_coords(colours[ordered]),
        hue_spread = max(.hue_difference(hcl[, "H"], mean_hue))
      )
    }, error = function(e) NULL)
  })
  catalogue <- Filter(Negate(is.null), catalogue)
  .catpalettes_cache[[key]] <- catalogue
  catalogue
}


#' Generate nested colours from parent colours
#'
#' Generate related colour shades for each group by matching
#' parent colours to sequential palettes or generating
#' shades using the HCL colour space.
#'
#' @param parent A named character vector of parent colours.
#' @param categories A vector of unique category names.
#' @param groups A vector specifying group membership.
#' @param max_delta_e Maximum acceptable colour distance
#'   between a parent colour and a candidate shade.
#' @param max_hue_spread Maximum acceptable hue spread
#'   within a candidate palette.
#' @param luminance Two numeric values specifying the
#'   luminance range.
#' @param sources A character vector specifying the palette
#'   sources to search.
#' @param background A single opaque background colour.
#' @return A named character vector of nested colours
#'   with group and report attributes.
#' @noRd
.nest_from_parent <- function(
    parent, categories, groups, max_delta_e = 10, max_hue_spread = 20,
    luminance = c(35, 85), sources = c("dynamic", "discrete"),
    background = "#FFFFFF") {
  parent <- .check_colours(parent)
  categories <- .check_categories(categories)
  groups <- as.character(groups)
  background <- .check_background(background)
  if (length(groups) != length(categories) || anyNA(groups)) {
    stop("Supply one group name for each category.", call. = FALSE)
  }
  if (is.null(names(parent)) || !all(unique(groups) %in% names(parent))) {
    stop("parent must be a named colour vector covering every group, ",
         "e.g. the output of select_palette().", call. = FALSE)
  }
  max_delta_e <- .check_threshold(max_delta_e)
  if (!is.numeric(luminance) || length(luminance) != 2L || anyNA(luminance) ||
      any(!is.finite(luminance)) || any(luminance < 0 | luminance > 100) || luminance[1] > luminance[2]) {
    stop("luminance must be two ordered values between 0 and 100.", call. = FALSE)
  }
  sources <- match.arg(sources, c("dynamic", "discrete"), several.ok = TRUE)

  catalogue <- .sequential_catalogue(sources, background)
  flat_parent <- .flatten_colours(parent, background)
  result <- stats::setNames(rep(NA_character_, length(categories)), categories)
  report <- list()

  for (group in unique(groups)) {
    members <- categories[groups == group]       # keeps input order
    k <- length(members)
    hcl <- .hcl_coords(flat_parent[[group]])
    row <- data.frame(
      group = group, parent = parent[[group]], n = k,
      hue = round(hcl[, "H"]), chroma = round(hcl[, "C"]),
      luminance = round(hcl[, "L"]), method = NA_character_,
      palette_id = NA_character_, delta_e = NA_real_, stringsAsFactors = FALSE
    )

    if (k == 1L) {
      result[members] <- parent[[group]]
      row$method <- "parent colour"
      report[[group]] <- row
      next
    }

    # Closest single-hue sequential palette with enough shades.
    parent_lab <- .lab_coords(flat_parent[[group]])
    best <- NULL
    best_delta <- Inf
    for (candidate in catalogue) {
      if (candidate$hue_spread > max_hue_spread ||
          length(candidate$colours) < k) next
      delta <- min(sqrt(colSums((t(candidate$lab) - parent_lab[1, ])^2)))
      if (delta < best_delta) {
        best <- candidate
        best_delta <- delta
      }
    }

    if (!is.null(best) && best_delta <= max_delta_e) {
      # k evenly spaced shades, darkest to the first category.
      result[members] <- best$colours[
        round(seq(1, length(best$colours), length.out = k))
      ]
      row$method <- paste(best$source, "palette")
      row$palette_id <- best$palette_id
      row$delta_e <- round(best_delta, 1)
    } else {
      result[members] <- grDevices::hcl(
        h = hcl[, "H"], c = hcl[, "C"],
        l = seq(luminance[1], luminance[2], length.out = k), fixup = TRUE
      )
      row$method <- "generated (parent hue + chroma)"
      if (!is.null(best)) row$delta_e <- round(best_delta, 1)
    }
    report[[group]] <- row
  }

  report <- do.call(rbind, report)
  rownames(report) <- NULL
  attr(result, "groups") <- stats::setNames(groups, categories)
  attr(result, "report") <- report
  result
}