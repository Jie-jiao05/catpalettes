# catpalettes: Quick User Guide

## 1. What functions do we provide?

`catpalettes` helps users find palettes, compare colours, and create consistent category–colour mappings.

| Our function | What it does | What it returns |
|---|---|---|
| `find_palettes()` | Finds existing palettes with enough colours and optionally applies colour-separation screening | A candidate table |
| `plot_palettes()` | Compares multiple palettes, including grayscale and colour-vision simulations | A comparison plot |
| `select_palette()` | Retrieves an existing palette or accepts custom colours, then assigns category names | A named colour vector |
| `group_palette()` | Generates related colours for categories within user-defined groups | A named colour vector with group information |
| `plot_palette()` | Displays one colour vector with category labels and optional grouping | A category-colour preview |
| `assess_palette()` | Measures the smallest pairwise colour difference under different views | An assessment table |
| `export_palette()` | Saves category names, original colour codes, and available group information | A CSV file |

**`plot_palettes()` compares several palettes. `plot_palette()` displays one palette and its category assignments.**

These functions use `paletteer` for existing palettes, `colorspace` for colour transformations, and `ggplot2` for plotting.

## 2. What do you need?

You need R, preferably RStudio, and the file containing our function definitions.

Install the dependencies once:

```r
install.packages(c("paletteer", "colorspace", "ggplot2"))
```

At the beginning of each R session, load the functions:

```r
source("R/cat_palettes.R")  # <- Change this to your file path.
library(ggplot2)
```

The current version loads from a script. Use `source()`, rather than `library(catpalettes)`.

If you cannot locate the script:

```r
source(file.choose())
```

## 3. Basic workflow: find, preview, select, and plot

### Step 1: Prepare your dataset

This example contains 15 fictional treatment categories.

```r
category_names <- paste("Treatment", 1:15)  # <- Change your category names.
participant_counts <- c(
  92, 74, 58, 41, 85, 63, 46, 76, 54, 39, 28, 68, 52, 36, 24
)  # <- Change your values: one per category.

stopifnot(
  length(category_names) == length(participant_counts),
  !anyDuplicated(category_names)
)

example_data <- data.frame(
  treatment = factor(category_names, levels = category_names),
  participants = participant_counts
)
```

This step uses base R to prepare data. It does not call our functions.

### Step 2: Find palettes with enough colours

Start with colour-count filtering only:

```r
# OUR FUNCTION: find existing qualitative palettes with enough colours.
candidates <- find_palettes(
  n = length(category_names),
  colourblind = FALSE,
  grayscale = FALSE
)

nrow(candidates)
head(candidates)
```

The candidate table contains:

| Column | Meaning |
|---|---|
| `palette_id` | Identifier used to select a palette |
| `n_available` | Number of colours in the original palette |
| `type` | Palette type |
| `n_selected` | Number of colours requested |
| `has_transparency` | Whether the selected colours include transparency |

Candidates are ordered by name, not by quality. If no candidates are returned, adjust your requirements before continuing.

### Step 3: Compare candidate palettes

```r
# OUR FUNCTION: compare candidates in RStudio's Plots pane.
plot_palettes(
  candidates,
  views = c("original", "deutan", "grayscale"),
  max_palettes = 8  # <- Change how many palettes to display.
)
```

Available preview options:

| View | Meaning |
|---|---|
| `"original"` | Original appearance on the specified background |
| `"grayscale"` | Grayscale preview |
| `"deutan"` | Deutan colour-vision simulation |
| `"protan"` | Protan colour-vision simulation |
| `"tritan"` | Tritan colour-vision simulation |

**Previewing a simulation does not filter candidates.** It only changes what you see.

### Step 4: Select a palette and assign category names

```r
selected_row <- 1L  # <- Change to your preferred row in candidates.
stopifnot(selected_row >= 1L, selected_row <= nrow(candidates))

# OUR FUNCTION: retrieve colours and bind them to category names.
my_colours <- select_palette(
  palette = candidates$palette_id[selected_row],
  categories = category_names
)

# OUR FUNCTION: inspect the resulting category-colour mapping.
plot_palette(my_colours)
```

The result is a named colour vector. The function uses the first 15 colours from the selected palette; it does not search for an optimal subset.

### Step 5: Assess your selected colours

```r
# OUR FUNCTION: measure colour separation under different views.
assessment <- assess_palette(
  my_colours,
  threshold = 10,         # <- Change the screening threshold.
  severity = 1,           # <- Change simulation severity: 0–1.
  background = "#FFFFFF"  # <- Match your plotting background.
)

assessment
```

Key output columns:

| Column | Meaning |
|---|---|
| `view` | Display condition being assessed |
| `min_delta_e` | Smallest pairwise CIELAB colour difference |
| `closest_pair` | The two colours with the smallest difference |
| `meets_threshold` | Whether the smallest difference reaches the threshold |

Assessment does not remove or modify colours. The default threshold of 10 is a heuristic, not an accessibility standard.

### Step 6: Apply the colours to your chart

This step uses existing `ggplot2` functions.

```r
p <- ggplot(
  example_data,
  aes(treatment, participants, fill = treatment)
) +
  geom_col() +
  scale_fill_manual(values = my_colours) +
  labs(x = NULL, y = "Participants", caption = "Fictional example data.") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", colour = NA)
  )

print(p)
```

The category names in your data must match `names(my_colours)`.

## 4. Alternative workflow: generate grouped colours

Use this when categories belong to larger groups.

```r
categories <- c("Drug A", "Drug B", "Surgery A", "Surgery B")
groups <- c("Medication", "Medication", "Surgery", "Surgery")

# OUR FUNCTION: generate related colours within each group.
grouped_colours <- group_palette(
  categories = categories,
  groups = groups
)

# OUR FUNCTION: preview the grouped assignments.
plot_palette(
  grouped_colours,
  views = c("original", "grayscale")
)

# OUR FUNCTION: assess the generated colours separately.
assess_palette(grouped_colours)
```

This workflow **generates new colours**. It does not reorganize a selected `paletteer` palette or inherit the candidate screening results.

To apply these colours, use:

```r
scale_fill_manual(values = grouped_colours)
```

Your chart must contain the corresponding category names. Save and reuse the complete mapping; regenerating colours from a smaller subset can change assignments.

## 5. Optional features

### Use your own colours

```r
# OUR FUNCTION: validate custom colours and assign category names.
custom_colours <- select_palette(
  colours = c("#0072B2", "#E69F00", "#009E73", "#CC79A7"),
  categories = c("A", "B", "C", "D")
)

plot_palette(custom_colours, views = c("original", "deutan", "grayscale"))
assess_palette(custom_colours)
```

### Screen candidates before selecting

```r
# OUR FUNCTION: retain only palettes that meet the requested screening criteria.
screened <- find_palettes(
  n = 8,                 # <- Change the number of categories.
  colourblind = TRUE,    # Require all three colour-vision simulations to pass.
  grayscale = FALSE,     # <- Set TRUE to also require grayscale separation.
  threshold = 10,        # <- Change the minimum colour difference.
  severity = 1,
  background = "#FFFFFF"
)

if (nrow(screened) > 0L) {
  plot_palettes(screened)
} else {
  message("No matches. Review your requirements before searching again.")
}
```

Strict requirements may produce no matches. In particular, requiring 15 grayscale colours to have a minimum pairwise difference of 10 is incompatible with the approximately 0–100 lightness range used by this assessment.

### Change the background

Transparent colours are retained in selected and exported colour vectors. For previews and assessments, they are composited onto the specified background.

```r
plot_palette(my_colours, background = "#202020")
assess_palette(my_colours, background = "#202020")
```

Use the same background for screening, previews, assessments, and the actual plotting panel. Fully transparent colours appear as the background.

## 6. Save your results

```r
# OUR FUNCTION: export the category-colour table.
export_palette(my_colours, "my_colour_mapping.csv")

# BASE R: preserve the complete colour object for reuse.
saveRDS(my_colours, "my_colours.rds")

# BASE R: restore the mapping in a later session.
my_colours <- readRDS("my_colours.rds")

# ggplot2: save your chart.
ggsave("my_chart.png", plot = p, width = 10, height = 6, dpi = 300, bg = "white")
```

Change the filenames as needed. Files are saved in the current working directory:

```r
getwd()
```

## 7. Common problems

| Problem | What to check |
|---|---|
| “Could not find function” | Load the function script with `source()`. |
| Script cannot be found | Check its path or use `source(file.choose())`. |
| No matching palettes | Review the category count and screening requirements. |
| Candidates were skipped | Inspect `attr(candidates, "skipped")`. |
| Only some palettes appear | Increase `max_palettes` or pass a subset of the candidate table. |
| A selected row does not exist | Choose a row between 1 and `nrow(candidates)`. |
| Colours do not match categories | Compare your data labels with `names(my_colours)`. |
| Final chart ignores the selected palette | Check whether `scale_fill_manual()` uses `my_colours` or `grouped_colours`. |

For a first run, follow **Section 3**. Explore grouped colours and stricter screening after the basic workflow works.
