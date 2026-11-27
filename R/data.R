#' Simulated veterinary patient data
#'
#' A simulated dataset of 1,000 animals treated at a veterinary
#' clinic. The dataset contains species and breed information
#' for demonstrating categorical and nested colour palettes.
#'
#' @format A data frame with 1,000 rows and 3 variables:
#' \describe{
#'   \item{id}{Unique animal identifier.}
#'   \item{species}{Factor indicating the animal species:
#'     Dog, Cat, Rabbit, Bird, or Reptile.}
#'   \item{breed}{Factor indicating the animal breed, with
#'     22 possible breed categories across the five species.}
#' }
#'
#' @source Simulated data generated for the catpalettes package
#'   using a random seed of 2026. All observations are fictional.
#'
#' @examples
#' data(vet_patients)
#' head(vet_patients)
#' table(vet_patients$species)
#' table(vet_patients$species, vet_patients$breed)
"vet_patients"

#' Movie genres and ratings
#'
#' A dataset containing movie titles, release years, genre
#' classifications, average ratings, and watch counts.
#' Movies may belong to multiple genres. The dataset can be
#' used to demonstrate colour palettes for categorical data.
#'
#' @format A data frame with 3,883 rows and 21 variables:
#' \describe{
#'   \item{Name}{Movie title, including the release year.}
#'   \item{ReleaseDate}{Year of release.}
#'   \item{Action}{Action genre indicator.}
#'   \item{Adventure}{Adventure genre indicator.}
#'   \item{Children}{Children's genre indicator.}
#'   \item{Comedy}{Comedy genre indicator.}
#'   \item{Crime}{Crime genre indicator.}
#'   \item{Documentary}{Documentary genre indicator.}
#'   \item{Drama}{Drama genre indicator.}
#'   \item{Fantasy}{Fantasy genre indicator.}
#'   \item{Noir}{Film noir genre indicator.}
#'   \item{Horror}{Horror genre indicator.}
#'   \item{Musical}{Musical genre indicator.}
#'   \item{Mystery}{Mystery genre indicator.}
#'   \item{Romance}{Romance genre indicator.}
#'   \item{SciFi}{Science fiction genre indicator.}
#'   \item{Thriller}{Thriller genre indicator.}
#'   \item{War}{War genre indicator.}
#'   \item{Western}{Western genre indicator.}
#'   \item{AvgRating}{Average movie rating.}
#'   \item{Watches}{Recorded watch count.}
#' }
#'
#' @details
#' Genre indicators are coded as 1 if a movie belongs to
#' the genre and 0 otherwise. A movie may belong to more
#' than one genre.
#'
#' @examples
#' data(movies)
#' head(movies)
#' table(movies$Comedy)
#' colSums(movies[, 3:19], na.rm = TRUE)
"movies"