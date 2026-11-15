# ============================================================
# STEP 1: LOAD THE FUNCTIONS AND REQUIRED PACKAGE
# ============================================================

source("R/cat_palettes.R")  # <- Change this to your script's file path.
library(ggplot2)


# ============================================================
# STEP 2: CREATE AN EXAMPLE DATASET
# These are fictional data. Replace them with your own data.
# ============================================================

treatment_names <- c(
  paste0("Drug ", 1:4),
  paste0("Surgery ", 1:3),
  paste0("Rehab ", 1:4),
  paste0("Other ", 1:4)
)  # <- Change these to your category names.

treatment_groups <- rep(
  c("Medication", "Surgery", "Rehabilitation", "Other"),
  times = c(4, 3, 4, 4)  # <- Change the number of categories in each group.
)

example_data <- data.frame(
  treatment = factor(treatment_names, levels = treatment_names),
  group = factor(treatment_groups, levels = unique(treatment_groups)),
  participants = c(
    92, 74, 58, 41, 85, 63, 46, 76, 54, 39, 28, 68, 52, 36, 24
  )  # <- Change these to your values: one value per category.
)

# Category names, groups, and values must have matching lengths.
head(example_data)


# ============================================================
# STEP 3: SET YOUR PALETTE REQUIREMENTS
# ============================================================

n_categories <- nlevels(example_data$treatment)  # Automatically counted from the data.
require_colourblind <- FALSE  # <- Set TRUE to require colour-vision screening.
require_grayscale <- FALSE   # <- Set TRUE to require grayscale screening.threshold <- 10              # <- Change the minimum colour-difference threshold.
severity <- 1                # <- Change simulation severity: 0 to 1.
background <- "#FFFFFF"      # <- Change the background colour.

preview_views <- c(
  "original", "deutan", "grayscale"
)  # <- Choose from: original, grayscale, deutan, protan, tritan.

# The threshold is a screening heuristic, not an accessibility certification.
# Requiring 15 colours to pass grayscale screening may return no matches.


# ============================================================
# STEP 4: FIND EXISTING PALETTES USING OUR FUNCTION
# find_palettes() returns a table of matching palettes.
# ============================================================

candidates <- find_palettes(
  n = n_categories,
  type = "qualitative",  # <- Change to "sequential", "divergent", or NULL if needed.
  colourblind = require_colourblind,
  grayscale = require_grayscale,
  threshold = threshold,
  severity = severity,
  background = background
)

head(candidates)
message("Matching palettes: ", nrow(candidates))


# ============================================================
# STEP 5: PREVIEW AND SELECT AN EXISTING PALETTE
# This step runs only when Step 4 finds matching palettes.
# ============================================================

max_to_show <- 8L  # <- Change how many candidate palettes to display.
selected_row <- 1L # <- Change to the candidate-table row you want to select.

# Clear any selection from an earlier run.
selected_colours <- NULL

if (nrow(candidates) > 0L) {

  # OUR FUNCTION: compare candidate palettes in one figure.
  print(plot_palettes(
    candidates,
    views = preview_views,
    max_palettes = max_to_show,
    severity = severity,
    background = background
  ))

  # Check that the selected row exists.
  if (length(selected_row) != 1L || !is.numeric(selected_row) ||
      is.na(selected_row) || selected_row < 1L ||
      selected_row > nrow(candidates) || selected_row != floor(selected_row)) {
    stop("selected_row must be a valid row number in candidates.")
  }

  # OUR FUNCTION: assign the selected palette to your category names.
  selected_colours <- select_palette(
    palette = candidates$palette_id[selected_row],
    categories = treatment_names
  )

  # OUR FUNCTION: show which colour belongs to each category.
  print(plot_palette(
    selected_colours,
    views = preview_views,
    severity = severity,
    background = background
  ))

} else {
  message(
    "No matching palettes. Change the requirements in Step 3 and rerun, ",
    "or continue to the independent grouped-colour workflow in Step 6."
  )
}


# ============================================================
# STEP 6: GENERATE GROUPED COLOURS AS AN ALTERNATIVE
# This creates NEW colours; it does not use the palette from Step 5.
# ============================================================

# OUR FUNCTION: assign a hue to each group and shades to its categories.
grouped_colours <- group_palette(
  categories = treatment_names,
  groups = treatment_groups,
  hues = c(
    Medication = 240,     # <- Change hue angle: 0–360.
    Surgery = 55,         # <- Change hue angle: 0–360.
    Rehabilitation = 145,# <- Change hue angle: 0–360.
    Other = 305           # <- Change hue angle: 0–360.
  ),                      # Group names must match treatment_groups.
  luminance = c(45, 80),   # <- Change the lightness range: 0–100.
  chroma = 45             # <- Change colour intensity: a non-negative number.
)

# OUR FUNCTION: preview the generated colours by group.
print(plot_palette(
  grouped_colours,
  views = preview_views,
  severity = severity,
  background = background
))


# ============================================================
# STEP 7: CHOOSE WHICH COLOURS TO USE AND ASSESS THEM
# The assessment below applies to the colours used in the final chart.
# ============================================================

colour_source <- "grouped"  # <- Change to "selected" to use the palette from Step 5.

if (colour_source == "grouped") {
  chart_colours <- grouped_colours
} else if (colour_source == "selected") {
  if (is.null(selected_colours)) {
    stop("No existing palette was selected. Complete Step 5 or use 'grouped'.")
  }
  chart_colours <- selected_colours
} else {
  stop("colour_source must be 'grouped' or 'selected'.")
}

# OUR FUNCTION: assess the chosen colours in all five views.
colour_assessment <- assess_palette(
  chart_colours,
  views = c("original", "deutan", "protan", "tritan", "grayscale"),
  threshold = threshold,
  severity = severity,
  background = background
)
print(colour_assessment)

# Identify the views required by your settings in Step 3.
required_views <- character()
if (require_colourblind) {
  required_views <- c(required_views, "deutan", "protan", "tritan")
}
if (require_grayscale) {
  required_views <- c(required_views, "grayscale")
}

# Create a clear screening-status message for the chart.
if (!length(required_views)) {
  screening_status <- "No colour-separation screening was required"
} else {
  required_results <- subset(colour_assessment, view %in% required_views)

  if (anyNA(required_results$meets_threshold)) {
    screening_status <- "Some required assessments could not be evaluated"
  } else if (all(required_results$meets_threshold)) {
    screening_status <- "Colours meet the requested numerical screening thresholds"
  } else {
    failed_views <- required_results$view[!required_results$meets_threshold]
    screening_status <- paste(
      "Below the requested threshold in:",
      paste(failed_views, collapse = ", ")
    )
  }
}
message(screening_status)


# ============================================================
# STEP 8: APPLY THE COLOURS TO YOUR DATA USING ggplot2
# catpalettes supplies colours; ggplot2 draws the chart.
# ============================================================

chart_title <- "Participants by treatment"  # <- Change your chart title.
y_axis_label <- "Participants"              # <- Change your y-axis label.

example_plot <- ggplot(
  example_data,
  aes(x = treatment, y = participants, fill = treatment)
) +
  geom_col(width = 0.75) +
  geom_text(aes(label = participants), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = chart_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  facet_grid(~group, scales = "free_x", space = "free_x") +
  labs(
    title = chart_title,
    subtitle = screening_status,
    x = NULL,
    y = y_axis_label,
    caption = "Fictional data. Numerical screening does not certify accessibility."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = background, colour = NA)
  )

print(example_plot)


# ============================================================
# STEP 9: SAVE YOUR RESULTS — OPTIONAL
# Remove # from the commands below to run them.
# Files are saved to your current working directory: getwd().
# ============================================================

# OUR FUNCTION: save the category-colour mapping.
# export_palette(chart_colours, "treatment_colours.csv")  # <- Change the filename.

# BASE R: save the colour object for reuse in later analyses.
# saveRDS(chart_colours, "treatment_colours.rds")  # <- Change the filename.

# BASE R: save the assessment table.
# write.csv(colour_assessment, "colour_assessment.csv", row.names = FALSE)

# ggplot2: save the chart.
# ggsave(
#   filename = "treatment_chart.png",  # <- Change the filename.
#   plot = example_plot,
#   width = 12, height = 6,            # <- Change dimensions in inches.
#   dpi = 300, bg = "white"            # <- Change resolution or outer background.
# )