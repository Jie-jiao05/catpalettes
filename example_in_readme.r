# ============================================================
# Example: Find a palette and apply it to a bar chart
# Run after loading the catpalettes function definitions.
# ============================================================

library(ggplot2)

# STEP 1: Create fictional data — replace with your own categories and values.
category_names <- paste("Treatment", 1:15)
participant_counts <- c(92, 74, 58, 41, 85, 63, 46, 76, 54, 39, 28, 68, 52, 36, 24)

stopifnot(
  length(category_names) == length(participant_counts),
  !anyDuplicated(category_names)
)

example_data <- data.frame(
  treatment = factor(category_names, levels = category_names),
  participants = participant_counts
)

# STEP 2: Find palettes with enough colours.
# OUR FUNCTION: screening is disabled here; we assess the selection in Step 5.
candidates <- find_palettes(
  n = length(category_names),
  colourblind = FALSE,
  grayscale = FALSE
)

message("Matching palettes: ", nrow(candidates))
print(head(candidates))
if (nrow(candidates) == 0L) stop("No matching palettes. Review your requirements.")

# STEP 3: Compare candidate palettes.
# OUR FUNCTION: previewing simulations does not filter the candidates.
print(plot_palettes(
  candidates,
  views = c("original", "deutan", "grayscale"),
  max_palettes = 8  # <- Change how many palettes to display.
))

# STEP 4: Select a palette and assign colours to category names.
selected_row <- 1L  # <- Change to your preferred row in candidates.
stopifnot(
  length(selected_row) == 1L,
  is.numeric(selected_row),
  is.finite(selected_row),
  selected_row == floor(selected_row),
  selected_row >= 1L,
  selected_row <= nrow(candidates)
)

# OUR FUNCTION: retrieve the colours and name them using your categories.
my_colours <- select_palette(
  palette = candidates$palette_id[selected_row],
  categories = category_names
)

# OUR FUNCTION: display each category's assigned colour.
print(plot_palette(my_colours))

# STEP 5: Assess colour separation.
# OUR FUNCTION: report results without changing the selected colours.
# The threshold is a screening heuristic, not an accessibility certification.
assessment <- assess_palette(
  my_colours,
  threshold = 10,         # <- Change the minimum colour-difference threshold.
  severity = 1,           # <- Change simulation severity: 0–1.
  background = "#FFFFFF"  # <- Match your chart background.
)
print(assessment)

# STEP 6: Apply the selected colours using ggplot2.
p <- ggplot(example_data, aes(treatment, participants, fill = treatment)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = my_colours) +
  labs(
    title = "Participants by treatment",
    subtitle = paste("Palette:", candidates$palette_id[selected_row]),
    x = NULL, y = "Participants",
    caption = "Fictional data. See the assessment table for colour-separation results."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA)
  )

print(p)