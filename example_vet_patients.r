# ============================================================
# Example: colour the vet_patients dataset
# Part A: 5 species  -> pick an existing palette (find_palettes)
# Part B: 22 breeds  -> shades of one hue per species (group_palette)
# Run from the project root.
# ============================================================

source("R/cat_palettes.R")
library(ggplot2)

load("data/vet_patients.rda")  # creates `vet_patients` (fictional data)
str(vet_patients)

# Count patients per species and per breed.
species_counts <- as.data.frame(table(species = vet_patients$species),
                                responseName = "patients")
breed_counts <- as.data.frame(table(breed = vet_patients$breed,
                                    species = vet_patients$species),
                              responseName = "patients")
breed_counts <- breed_counts[breed_counts$patients > 0, ]  # each breed has one species


# ============================================================
# PART A: SPECIES (5 categories) — existing palette
# ============================================================

species_names <- levels(vet_patients$species)

# OUR FUNCTION: palettes with 5+ colours that pass colour-vision screening.
candidates <- find_palettes(
  n = length(species_names),
  type = "qualitative",
  colourblind = TRUE,   # <- Set FALSE to see more candidates.
  grayscale = FALSE
)
message("Matching palettes: ", nrow(candidates))
if (nrow(candidates) == 0L) stop("No matching palettes. Relax the requirements.")

# OUR FUNCTION: compare the first few candidates.
print(plot_palettes(candidates, views = c("original", "deutan"), max_palettes = 6))

# OUR FUNCTION: name the colours after the species.
species_colours <- select_palette(
  palette = candidates$palette_id[1],  # <- Change the row to pick another palette.
  categories = species_names
)
print(assess_palette(species_colours))

print(
  ggplot(species_counts, aes(species, patients, fill = species)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = patients), vjust = -0.4) +
    scale_fill_manual(values = species_colours) +
    labs(title = "Patients by species", x = NULL, y = "Patients",
         caption = "Fictional data.") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
)


# ============================================================
# PART B: BREEDS (22 categories) grouped by species
# ============================================================

breed_colours <- group_palette(
  categories = as.character(breed_counts$breed),
  groups = as.character(breed_counts$species),
  hues = c(Dog = 30, Cat = 250, Rabbit = 130, Bird = 80, Reptile = 320),
  luminance = c(40, 82),
  chroma = 50
)

# OUR FUNCTION: preview the breed colours, grouped by species.
print(plot_palette(breed_colours, views = c("original", "deutan")))

# Expect some breeds to fall below the threshold: shades within one hue are
# intentionally close. That is the trade-off of grouped colours.
print(assess_palette(breed_colours))

print(
  ggplot(breed_counts, aes(reorder(breed, -patients), patients, fill = breed)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = breed_colours) +
    facet_grid(~species, scales = "free_x", space = "free_x") +
    labs(title = "Patients by breed", x = NULL, y = "Patients",
         caption = "Fictional data. Colours: one hue per species.") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank()
    )
)
