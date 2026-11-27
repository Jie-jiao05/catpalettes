#' Compare multiple colour palettes
#'
#' Display and compare colour palettes under different colour
#' vision conditions.
#'
#' @param palettes A data frame containing a palette_id column
#'   (e.g. from find_palettes()), a character vector of colours,
#'   or a list of colour vectors.
#' @param n Number of colours to select from each palette.
#'   If NULL, uses n_selected from the data frame when
#'   available. Otherwise, all colours are used.
#' @param views A character vector of display conditions.
#'   Options are "original", "grayscale", "deutan",
#'   "protan" and "tritan".
#' @param severity Severity of simulated colour vision deficiency,
#'   ranging from 0 to 1.
#' @param max_palettes Maximum number of palettes to plot.
#'   Defaults to 12.
#' @param background A single opaque background colour.
#'
#' @return A ggplot object.
#'
#' @details
#' If the number of palettes exceeds max_palettes, only the
#' first max_palettes are plotted.
#' Palette names must be non-empty and unique. If no names
#' are supplied for a list of palettes, names are assigned
#' automatically.
#'
#' @seealso [find_palettes()], [plot_palette()], [assess_palette()]
#' @export
#'
#' @examples
#' plot_palettes(
#'   list(
#'     Palette1 = c("red", "blue", "green"),
#'     Palette2 = c("purple", "orange", "pink")
#'   )
#' )
#'
#' candidates <- find_palettes(n = 5)
#' plot_palettes(candidates, max_palettes = 3)
#'
#' plot_palettes(
#'   list(
#'     Palette1 = c("red", "blue", "green"),
#'     Palette2 = c("purple", "orange", "pink")
#'   ),
#'   views = c("original", "deutan", "grayscale")
#' )
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