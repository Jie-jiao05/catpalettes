#' Plot a single colour palette
#'
#' Display category colours under different colour vision
#' conditions, with optional grouping of categories.
#'
#' @param colours A character vector of valid colours.
#' @param groups An optional vector of group names, one per
#'   category. Can be named by category. If NULL, stored
#'   group information is used when available.
#' @param views A character vector of display conditions.
#'   Options are "original", "grayscale", "deutan",
#'   "protan" and "tritan".
#' @param severity Severity of simulated colour vision deficiency,
#'   ranging from 0 to 1.
#' @param background A single opaque background colour.
#'
#' @return A ggplot object.
#'
#' @seealso [assess_palette()], [plot_palettes()], [nest_palette()]
#' @export
#'
#' @examples
#' plot_palette(c("red", "blue", "green"))
#'
#' plot_palette(
#'   colours = c(A = "#D0006F", B = "#8B84D7", C = "#0072B2"),
#'   views = c("original", "deutan", "grayscale")
#' )
#' 
#' plot_palette(
#'   colours = c("lightblue", "darkblue", "pink", "red"),
#'   groups = c("A", "A", "B", "B")
#' )
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
