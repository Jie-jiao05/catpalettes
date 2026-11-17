# catpalettes: Quick User Guide

Find colour palettes, compare their appearance, and assign related colours to grouped categories. This guide uses the simulated veterinary dataset `vet_patients.rda`.

## 1. What functions do we provide?

| Function | What it does | Returns |
|---|---|---|
| `find_palettes()` | Lists discrete palettes with at least the requested number of colours; optionally filters by type | Candidate table |
| `plot_palettes()` | Compares several palettes in original, grayscale, or colour-vision simulation views | ggplot object |
| `select_palette()` | Retrieves existing or custom colours and assigns category names | Named colour vector |
| `nest_palette()` | Creates grouped colours, with or without existing parent colours | Named colour vector with group information and a report |
| `plot_palette()` | Previews one category–colour mapping, including groups | ggplot object |
| `assess_palette()` | Optionally measures colour separation under different viewing conditions | Assessment table |
| `export_palette()` | Saves category names, colour codes, and available group information | CSV file |


## 2. Setup

This version is loaded from an R script. Use the revised script supplied with this guide, rather than an older copy.

Install dependencies once:

```r
install.packages(c("paletteer", "colorspace", "ggplot2"))
```

The examples assume these files are in your project:

```text
R/catpalettes.R
data/vet_patients.rda
```

Run the following steps from your project folder. If your script has another name, change the `source()` path.

## 3. Veterinary example: select species colours, then create breed colours

### Step 1: Load the functions and data

The dataset contains simulated veterinary patients. We use `species` as the parent group and `breed` as the category within each group.

```r
source("R/catpalettes.R")  # <- Change to your function script's path.
library(ggplot2)

load("data/vet_patients.rda")  # <- Change to your dataset's path.
head(vet_patients)
```

### Step 2: Count patients by species and breed

This uses base R to prepare chart data; it does not call catpalettes functions. Zero-count combinations are removed.

```r
species_counts <- as.data.frame(table(species = vet_patients$species))
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
```

### Step 3: Find palettes with enough colours

```r
# OUR FUNCTION: list palettes with at least one colour per species.
candidates <- find_palettes(
  n = nrow(species_counts),
  type = "qualitative"  # <- Use NULL to include all discrete palette types.
)

nrow(candidates)
head(candidates)

if (nrow(candidates) == 0L) {
  stop("No matching palettes. Try type = NULL or request fewer colours.")
}
```

By default, `find_palettes(n = ...)` searches all types in paletteer's **discrete catalogue**. It does not search dynamic or continuous catalogues, assess colour separation, or reject similar grayscale colours. Qualitative palettes are intended for unordered categories, so this example requests that type explicitly.

| Candidate column | Meaning |
|---|---|
| `palette_id` | Identifier to pass to `select_palette()` |
| `n_available` | Number of colours listed in the original palette |
| `type` | Palette type |
| `n_selected` | Number of colours requested |

Candidates are ordered by name, not ranked by quality. Search lists catalogue entries; colours are retrieved when you select or preview a palette.

### Step 4: Compare candidates and select species colours

```r
# OUR FUNCTION: compare candidate palettes before choosing one.
print(plot_palettes(
  candidates,
  views = c("original", "grayscale"),  # <- Add "deutan" for another view.
  max_palettes = 8                     # <- Change the preview limit.
))

# OUR FUNCTION: assign the chosen palette's colours to species names.
species_colours <- select_palette(
  palette = candidates$palette_id[1],  # <- Change 1 to your preferred row.
  categories = as.character(species_counts$species)
)

# OUR FUNCTION: preview the selected category-colour mapping.
print(plot_palette(species_colours))
```

You choose the palette by changing the candidate row. `select_palette()` takes the first required number of colours; it does not find an optimal subset. After choosing another row, rerun selection and any later steps that use its output.

`max_palettes` limits only the preview, not the candidate table. Preview a subset of rows to browse further, or use `max_palettes = nrow(candidates)` to display all matches. Large comparisons may be difficult to read.

Both preview functions default to original and grayscale views. Available views are `"original"`, `"grayscale"`, `"deutan"`, `"protan"`, and `"tritan"`. Similar gray shades are a possible result of the conversion, not automatically a display error. Previews do not apply pass/fail checks or certify accessibility.

### Step 5: Apply the species colours to a chart

This step uses ggplot2. The named mapping from `select_palette()` controls the fills.

```r
species_plot <- ggplot(
  species_counts, aes(species, patients, fill = species)
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
```

### Step 6: Create breed colours using the selected species colours

**With `parent`**, `nest_palette()` searches for related shades based on existing group colours. If no suitable palette is found, it generates shades using the parent's hue and chroma. A group with one category receives its parent colour.

```r
# OUR FUNCTION: create breed colours related to their species colour.
breed_colours <- nest_palette(
  parent = species_colours,
  categories = as.character(breed_counts$breed),
  groups = as.character(breed_counts$species),
  max_delta_e = 10,       # <- Lower for stricter parent-colour matching.
  luminance = c(35, 85)   # <- Set the lightness range for generated fallback shades.
)

print(plot_palette(breed_colours))

# Show whether each group used a matched palette or generated shades.
attr(breed_colours, "report")
```

Group labels must match names in `parent`. Category names must be unique. The report's `delta_e` describes parent matching, not separation between the resulting breed colours. Related shades are not guaranteed to be distinguishable in every viewing condition.

### Step 7: Apply the breed colours to a chart

```r
breed_plot <- ggplot(
  breed_counts, aes(breed, patients, fill = breed)
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
```

## 4. Alternative: generate grouped colours without a parent

Use this instead of Step 6 when you do not need to inherit the species palette.

```r
# OUR FUNCTION: generate grouped colours independently of species_colours.
generated_colours <- nest_palette(
  categories = as.character(breed_counts$breed),
  groups = as.character(breed_counts$species),
  hues = c(
    Dog = 250, Cat = 30, Rabbit = 130, Bird = 80, Reptile = 320
  ),                     # <- Choose a hue for each species; omit for automatic hues.
  luminance = c(35, 85),  # <- Set the darker-to-lighter range.
  chroma = 45            # <- Set colour intensity.
)

print(plot_palette(generated_colours))
attr(generated_colours, "report")
```

To apply this alternative, change the fill scale in Step 7 to:

```r
scale_fill_manual(values = generated_colours)
```

Then recreate the chart. Creating `generated_colours` alone does not change an existing chart.

| Mode | Colour source | Relevant settings |
|---|---|---|
| With `parent` | Existing named group colours | `max_delta_e`, `max_hue_spread`, `sources`; `luminance` for fallback shades |
| Without `parent` | Newly generated HCL colours | `hues`, `chroma`, `luminance` |

Without `parent`, omitting `hues` assigns evenly spaced hues to sorted group names. This is deterministic, not random. The same inputs and settings reproduce the same colours. It does not select a parent palette from paletteer.

Do not supply `hues` or `chroma` with `parent`. Matching settings require `parent`. Save and reuse the full colour mapping: regenerating it from a subset can change assignments.

## 5. Optional assessment and custom colours

Assessment is separate from search and preview. It reports results without removing or changing colours.

```r
# OUR FUNCTION: check colour separation under different viewing conditions.
species_assessment <- assess_palette(
  species_colours,
  threshold = 10,         # <- Higher values require greater colour separation.
  severity = 1,           # <- Colour-vision simulation severity: 0 = none, 1 = full.
  background = "#FFFFFF"  # <- Match your chart's background.
)

species_assessment
```

| Assessment column | Meaning |
|---|---|
| `view` | Viewing condition |
| `min_delta_e` | Smallest pairwise CIELAB Delta E 1976 colour difference |
| `closest_pair` | Categories with the most similar colours |
| `threshold` | Requested minimum colour difference |
| `meets_threshold` | Whether that minimum is reached |
| `severity` | Requested colour-vision simulation severity |
| `background` | Background used for evaluation |

The threshold is a heuristic, not an accessibility standard. Severity affects colour-vision simulations, not original or grayscale views.

You can also use your own species colours:

```r
# OUR FUNCTION: validate custom colours and attach species names.
custom_colours <- select_palette(
  colours = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00"),
  categories = c("Dog", "Cat", "Rabbit", "Bird", "Reptile")
)

print(plot_palette(custom_colours))
```

Transparent colours are preserved in selected and exported mappings. Previews and assessments first composite them onto `background`. Match that setting to the actual chart background; fully transparent colours appear as the background.

## 6. Save your results

Uncomment the lines you want to run. Change the filenames as needed.

```r
# OUR FUNCTION: save the breed category-colour mapping as CSV.
# export_palette(breed_colours, file = "breed_colours.csv")

# BASE R: preserve colours and their group/report attributes.
# saveRDS(breed_colours, "breed_colours.rds")
# breed_colours <- readRDS("breed_colours.rds")

# ggplot2: save the breed chart.
# ggsave("breed_chart.png", breed_plot,
#        width = 12, height = 6, dpi = 300, bg = "white")
```

Files are saved relative to the current working directory, shown by `getwd()`.
