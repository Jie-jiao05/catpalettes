# ============================================================
# STEP 1: Load catpalettes and the veterinary dataset
# Run this code from your project's main folder.
# ============================================================

source("R/cat_palettes.R")
library(ggplot2)

load("data/vet_patients.rda")  
head(vet_patients)
#about remove one will still function
#library(dplyr)
#vet_patients |> group_by(species, breed) |> tally()
#vet_patients$species[vet_patients$breed == "Poodle"] <- NA
#vet_patients$breed[vet_patients$breed == "Persian"] <- NA
# ============================================================
# STEP 2: Count patients by species and breed
# These summaries provide the categories and values for our charts.
# ============================================================

species_counts <- as.data.frame(table(
  species = vet_patients$species
))
names(species_counts)[2] <- "patients"
species_counts <- subset(species_counts, patients > 0)

breed_counts <- as.data.frame(table(
  species = vet_patients$species,
  breed = vet_patients$breed
))
names(breed_counts)[3] <- "patients"
breed_counts <- subset(breed_counts, patients > 0)

species_counts
head(breed_counts)


# ============================================================
# STEP 3: Find and compare palettes for the species
# OUR FUNCTION: find_palettes() finds palettes with enough colours.
# OUR FUNCTION: plot_palettes() previews their appearance.
# Grayscale is displayed without rejecting similar shades.
# ============================================================

candidates <- find_palettes(
  n = nrow(species_counts),
  type = "qualitative"  # <- Use NULL to include all discrete palette types.
)

head(candidates)
nrow(candidates)

if (nrow(candidates) == 0L) {
  stop("No matching palettes. Try type = NULL or request fewer colours.")
}

print(plot_palettes(
  candidates,
  views = c("original", "grayscale"),  # <- Add "deutan" for another view.
  max_palettes = 15                     # <- Change how many candidates to preview.
))


# ============================================================
# STEP 4: Choose species colours and use them in a chart
# OUR FUNCTION: select_palette() assigns colours to category names.
# Change the candidate row to choose a different palette.
# ggplot2 uses the mapping through scale_fill_manual().
# ============================================================

species_colours <- select_palette(
  palette = candidates$palette_id[5],  # <- Change 1 to your preferred row.
  categories = as.character(species_counts$species)
)

print(plot_palette(species_colours))

species_plot <- ggplot(
  species_counts,
  aes(x = species, y = patients, fill = species)
) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = species_colours) +
  labs(
    title = "Veterinary patients by species",
    x = NULL, y = "Number of patients",
    caption = "Simulated example data."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", colour = NA)
  )

print(species_plot)


# ============================================================
# STEP 5: Create breed colours related to the species colours
# OUR FUNCTION: nest_palette(), WITH parent.
# Each breed receives a shade related to its species colour.
# If no suitable palette is found, the function generates shades.
# ============================================================

breed_colours <- nest_palette(
  parent = species_colours,
  categories = as.character(breed_counts$breed),
  groups = as.character(breed_counts$species),
  max_delta_e = 10,       # <- Lower for stricter matching to parent colours.
  luminance = c(35, 85)   # <- Set the lightness range for generated shades.
)

print(plot_palette(
  breed_colours,
  views = c("original", "grayscale")
))

# Show how the colours were obtained for each species.
attr(breed_colours, "report")

breed_plot <- ggplot(
  breed_counts,
  aes(x = breed, y = patients, fill = breed)
) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = breed_colours) +
  facet_grid(~species, scales = "free_x", space = "free_x") +
  labs(
    title = "Veterinary patients by breed",
    subtitle = "Breed colours are related to the selected species colours",
    x = NULL, y = "Number of patients",
    caption = "Simulated example data."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA)
  )

print(breed_plot)


# ============================================================
# STEP 6: Alternatively, generate breed colours without a parent
# OUR FUNCTION: nest_palette(), WITHOUT parent.
# This replaces group_palette() and does not use species_colours.
# Omit hues below if you want automatic group hues.
# ============================================================

generated_colours <- nest_palette(
  categories = as.character(breed_counts$breed),
  groups = as.character(breed_counts$species),
  hues = c(
    Dog = 250, Cat = 30, Rabbit = 130, Bird = 80, Reptile = 320
  ),                     # <- Choose a hue for each species.
  luminance = c(35, 85),  # <- Set the darker-to-lighter range.
  chroma = 45            # <- Set colour intensity.
)

print(plot_palette(
  generated_colours,
  views = c("original", "grayscale")
))

# Apply this alternative mapping to a new version of the breed chart.
# The original breed_plot keeps its existing colours.
generated_plot <- breed_plot

# Remove the existing fill scale before applying the alternative.
generated_plot$scales$scales <- Filter(
  function(scale) !"fill" %in% scale$aesthetics,
  generated_plot$scales$scales
)

generated_plot <- generated_plot +
  scale_fill_manual(values = generated_colours) +
  labs(subtitle = "Breed colours generated from chosen species hues")

print(generated_plot)


# ============================================================
# STEP 7: Optionally assess or export your colours
# OUR FUNCTION: assess_palette() reports colour separation only.
# OUR FUNCTION: export_palette() saves the category-colour mapping.
# Uncomment the lines you want to run.
# ============================================================

# assess_palette(breed_colours, threshold = 10)

# export_palette(species_colours, file = "species_colours.csv")
# export_palette(breed_colours, file = "breed_colours.csv")

# ggplot2::ggsave(
#   "breed_chart.png", breed_plot,
#   width = 12, height = 6, dpi = 300, bg = "white"
# )