# catpalettes
# Seven public functions for palette selection, preview, nesting and export.
# find_palettes | select_palette | nest_palette | plot_palette
# plot_palettes | assess_palette (optional) | export_palette
# Save as "catpalettes.R" and load with source("catpalettes.R").
# Install dependencies once:
# install.packages(c("paletteer", "colorspace", "ggplot2"))

# ---------- Validation ----------

.check_dependencies <- function() {
  packages <- c("paletteer", "colorspace", "ggplot2")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install these packages first: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}
.check_dependencies()

.check_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      !is.finite(n) || n < 1 || n != floor(n) || n > .Machine$integer.max) {
    stop("n must be a positive whole number within R's integer range.", call. = FALSE)
  }
  as.integer(n)
}

.check_severity <- function(severity) {
  if (!is.numeric(severity) || length(severity) != 1L || is.na(severity) ||
      !is.finite(severity) || severity < 0 || severity > 1) {
    stop("severity must be between 0 and 1.", call. = FALSE)
  }
  severity
}

.check_threshold <- function(threshold) {
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      !is.finite(threshold) || threshold < 0) {
    stop("threshold must be a finite non-negative number.", call. = FALSE)
  }
  threshold
}

.check_views <- function(views) {
  allowed <- c("original", "grayscale", "deutan", "protan", "tritan")
  if (!is.character(views) || !length(views) || anyNA(views) ||
      any(!views %in% allowed)) {
    stop("views must contain: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  unique(views)
}

.check_categories <- function(categories) {
  categories <- as.character(categories)
  if (!length(categories) || anyNA(categories) ||
      any(!nzchar(trimws(categories))) || anyDuplicated(categories)) {
    stop("Category names must be non-empty, unique, and non-missing.", call. = FALSE)
  }
  categories
}

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

# ---------- Transparency and display transformations ----------
# Original colour vectors retain their alpha values.
# Previews and assessments composite colours onto one opaque background.

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

# ---------- 1. Select an existing or custom palette ----------
# Takes the first n colours without interpolation and preserves transparency.

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

# ---------- 2. Assess colour separation ----------
# Uses minimum pairwise CIELAB Delta E 1976.
# The threshold is a screening heuristic, not accessibility certification.
# Scores depend on the background and simulation settings.

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

# ---------- 3. Find candidate palettes ----------
# Transparent colours are retained.
# Search uses colour count and optional palette type only; no separation screening.
# Candidates are ordered by name, not ranked by quality.

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

# ---------- 4. Compare palettes in one figure ----------
# Accepts a candidate table, a named list of palettes, or a colour vector.
# Defaults to the first 12 palettes; increase max_palettes to show more.

plot_palettes <- function(
    palettes, n = NULL, views = c("original", "grayscale"), severity = 1,
    max_palettes = 12, background = "#FFFFFF") {
  max_palettes <- .check_n(max_palettes)
  views <- .check_views(views)
  severity <- .check_severity(severity)
  background <- .check_background(background)
  if (!is.null(n)) n <- .check_n(n)

  if (is.data.frame(palettes)) {
    if (!"palette_id" %in% names(palettes)) stop("The table must contain palette_id.")
    if (!nrow(palettes)) stop("There are no candidate palettes to plot.")
    total <- nrow(palettes)
    chosen <- utils::head(palettes, max_palettes)
    sizes <- if (!is.null(n)) {
      rep(n, nrow(chosen))
    } else if ("n_selected" %in% names(chosen)) {
      chosen$n_selected
    } else {
      rep(NA_integer_, nrow(chosen))
    }
    palette_list <- lapply(seq_len(nrow(chosen)), function(i) {
      size <- if (is.na(sizes[i])) NULL else sizes[i]
      select_palette(as.character(chosen$palette_id[i]), n = size)
    })
    names(palette_list) <- as.character(chosen$palette_id)
  } else {
    if (is.character(palettes)) palettes <- list("Custom palette" = palettes)
    if (!is.list(palettes) || !length(palettes)) {
      stop("Supply candidates, a colour vector, or a list of palettes.")
    }
    total <- length(palettes)
    palette_list <- utils::head(palettes, max_palettes)
    if (is.null(names(palette_list))) {
      names(palette_list) <- paste("Palette", seq_along(palette_list))
    }
    palette_list <- lapply(palette_list, function(x) select_palette(colours = x, n = n))
  }

  if (anyNA(names(palette_list)) || any(!nzchar(names(palette_list))) ||
      anyDuplicated(names(palette_list))) {
    stop("Palette names must be non-empty and unique.")
  }

  rows <- list()
  index <- 1L
  for (palette_name in names(palette_list)) {
    for (view in views) {
      transformed <- .transform_colours(
        palette_list[[palette_name]], view, severity, background
      )
      rows[[index]] <- data.frame(
        palette = palette_name, position = seq_along(transformed),
        colour = unname(transformed), view = view, stringsAsFactors = FALSE
      )
      index <- index + 1L
    }
  }
  plot_data <- do.call(rbind, rows)
  plot_data$palette <- factor(plot_data$palette, levels = rev(names(palette_list)))
  plot_data$view <- factor(plot_data$view, levels = views)
  backgrounds <- data.frame(
    view = factor(views, levels = views),
    colour = vapply(views, function(view) {
      unname(.transform_colours(background, view, severity, background))
    }, character(1))
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(position, palette, fill = colour)) +
    ggplot2::geom_rect(
      data = backgrounds, ggplot2::aes(fill = colour),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, inherit.aes = FALSE
    ) +
    ggplot2::geom_tile(width = 0.94, height = 0.72, colour = "#888888", linewidth = 0.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_wrap(~view, ncol = min(2L, length(views))) +
    ggplot2::labs(
      title = "catpalettes: Palette comparison",
      subtitle = sprintf("Showing %s of %s palettes", length(palette_list), total),
      x = "Colour position", y = NULL,
      caption = paste0(
        "Colours composited on ", background,
        " before simulation. Previews do not certify accessibility."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
}

# ---------- Internal helper: generate grouped colours ----------
# Generates new HCL colours rather than reorganizing a paletteer palette.
# Reuse the full returned mapping to preserve colours across subsets.

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

# ---------- 6. Preview category assignments ----------

plot_palette <- function(
    colours, groups = NULL, views = c("original", "grayscale"),
    severity = 1, background = "#FFFFFF") {
  stored_groups <- attr(colours, "groups")
  colours <- .check_colours(colours)
  views <- .check_views(views)
  severity <- .check_severity(severity)
  background <- .check_background(background)
  categories <- names(colours)
  if (is.null(categories)) categories <- paste("Category", seq_along(colours))
  categories <- .check_categories(categories)

  if (is.null(groups)) groups <- stored_groups
  if (is.null(groups)) {
    groups <- rep("Categories", length(colours))
  } else if (!is.null(names(groups))) {
    if (anyDuplicated(names(groups)) || !all(categories %in% names(groups))) {
      stop("Named groups must cover every category without duplicate names.")
    }
    groups <- groups[categories]
  }
  groups <- as.character(groups)
  if (length(groups) != length(colours) || anyNA(groups) ||
      any(!nzchar(trimws(groups)))) {
    stop("Supply one valid group for every colour.")
  }

  plot_data <- do.call(rbind, lapply(views, function(view) {
    data.frame(
      category = categories, group = groups,
      colour = unname(.transform_colours(colours, view, severity, background)),
      view = view, stringsAsFactors = FALSE
    )
  }))
  plot_data$category <- factor(plot_data$category, levels = rev(categories))
  plot_data$group <- factor(plot_data$group, levels = unique(groups))
  plot_data$view <- factor(plot_data$view, levels = views)
  backgrounds <- data.frame(
    view = factor(views, levels = views),
    colour = vapply(views, function(view) {
      unname(.transform_colours(background, view, severity, background))
    }, character(1))
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = 1, y = category, fill = colour)) +
    ggplot2::geom_rect(
      data = backgrounds, ggplot2::aes(fill = colour),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, inherit.aes = FALSE
    ) +
    ggplot2::geom_tile(width = 0.8, height = 0.8, colour = "#888888", linewidth = 0.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_grid(group ~ view, scales = "free_y", space = "free_y") +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::labs(
      title = "catpalettes: Category colour assignment", x = NULL, y = NULL,
      caption = paste0(
        "Preview background: ", background,
        ". Original colour vectors retain transparency."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(angle = 0),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
}

# ---------- 7. Export the original colour mapping ----------
# Hex values include alpha; transparency is preserved.

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
# ---------- Internal helpers: match shades to parent colours ----------
# For each group, reads the hue and chroma of the group's parent colour
# (e.g. the colour assigned to each species), then looks for an existing
# single-hue sequential palette (paletteer dynamic or discrete) containing
# a colour close to it. If none is close enough, shades are generated from
# the parent's hue and chroma across the luminance range.

.hcl_coords <- function(colours) {
  colourspace <- methods::as(colorspace::hex2RGB(colours), "polarLUV")
  coords <- colorspace::coords(colourspace)
  coords[is.na(coords[, "H"]), "H"] <- 0
  coords
}

.lab_coords <- function(colours) {
  rgb <- t(grDevices::col2rgb(colours)) / 255
  grDevices::convertColor(rgb, from = "sRGB", to = "Lab", scale.in = 1)
}

.hue_difference <- function(a, b) {
  d <- abs(a - b) %% 360
  pmin(d, 360 - d)
}

.catpalettes_cache <- new.env(parent = emptyenv())

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

# ---------- Nest colours within groups ----------
# With parent: match existing shades or generate shades from parent colours.
# Without parent: generate grouped HCL colours using automatic or chosen hues.
# Both modes return named colours with groups and report attributes.

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
