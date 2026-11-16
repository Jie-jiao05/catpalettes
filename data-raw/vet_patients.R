# Creates data/vet_patients.rda: a hypothetical dataset of 1,000 animals
# treated at a vet clinic. All values are simulated (fictional).
# Run from the project root: source("data-raw/vet_patients.R")

set.seed(2026)

# Breeds within each species, with relative frequencies.
breeds <- list(
  Dog = c(
    "Labrador Retriever" = 0.22, "German Shepherd" = 0.15,
    "Golden Retriever" = 0.14, "French Bulldog" = 0.12,
    "Beagle" = 0.10, "Poodle" = 0.09, "Dachshund" = 0.08,
    "Mixed breed" = 0.10
  ),
  Cat = c(
    "Domestic Shorthair" = 0.40, "Domestic Longhair" = 0.15,
    "Siamese" = 0.12, "Maine Coon" = 0.12, "Persian" = 0.11,
    "Ragdoll" = 0.10
  ),
  Rabbit = c(
    "Holland Lop" = 0.40, "Netherland Dwarf" = 0.35, "Lionhead" = 0.25
  ),
  Bird = c(
    "Budgerigar" = 0.45, "Cockatiel" = 0.35, "African Grey" = 0.20
  ),
  Reptile = c(
    "Bearded Dragon" = 0.55, "Leopard Gecko" = 0.45
  )
)

species_prob <- c(Dog = 0.50, Cat = 0.33, Rabbit = 0.08, Bird = 0.05, Reptile = 0.04)

n <- 1000L
species <- sample(names(species_prob), n, replace = TRUE, prob = species_prob)
breed <- vapply(
  species,
  function(s) sample(names(breeds[[s]]), 1L, prob = breeds[[s]]),
  character(1),
  USE.NAMES = FALSE
)

vet_patients <- data.frame(
  id = seq_len(n),
  species = factor(species, levels = names(species_prob)),
  breed = factor(breed, levels = unlist(lapply(breeds, names), use.names = FALSE))
)

save(vet_patients, file = "data/vet_patients.rda", compress = "xz")
