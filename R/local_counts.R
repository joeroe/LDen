#' Title
#'
#' Local_counts is a function to find the counts of points of different
#' types within the local neighborhood of each point -- points are
#' included in the counts of their own neighborhood.
#' The output is a dataframe that includes the point location and
#' type of each point, as well as counts (by type) of points within the
#' specified radius of each point.
#'
#' @param location_data expects a data.frame with three columns:
#' \describe{
#'   \item{x}{Easting / x-coordinate of location}
#'   \item{y}{Northing / y-coordinate of location}
#'   \item{type}{categorical variable for type of evidence at location}
#' }
#' @param x Index of column containing the x-coordinate
#' @param y Index of column containing the y-coordinate
#' @param radius *numeric*. the radius of what constitues the
#' "neighborhood" of each point
#'
#'
#' @return a data.frame that includes the point location and
#' type of each point, as well as counts (by type) of points within the
#' specified radius of each point
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   locations <- LDen::AZ_A1020_BLM
#'   local_counts <- local_counts(locations, radius = 2)
#' }
#'
local_counts <- function(location_data, x = 1, y = 2, radius) {

  stopifnot(all(is.numeric(location_data[,y]), is.numeric(location_data[,x])))
  stopifnot(is.numeric(radius))

  # calculate distance matrix
  distance <- as.matrix(stats::dist(location_data[,c(x,y)], method = "euclidean"))

  output <- location_data
  output$radius <- radius

  # find the counts for artifacts of each type in the neighborhood of each point
  type_list <- sort(unique(location_data$type))
  num_types <- length(type_list)
  neighbor_counts_per_type <- lapply(type_list, function(current_type) {
    is_current_type <- location_data$type == current_type
    apply(distance, 1, function(one_row) {
      relevant_neighbors <- one_row[is_current_type]
      sum(relevant_neighbors <= radius)
    })
  })

  # reshape to named columns
  output <- cbind(output, neighbor_counts_per_type)
  colnames(output)[(ncol(output)-num_types+1):ncol(output)] <- paste0("count_", as.character(type_list))

  # add total count of neighboring artifacts
  output$count_total <- local_count(location_data[,x], location_data[,y], radius)

  return(output)
}

#' Count points within a radius
#'
#' For a vector of points specified by x and y coordinates, calculates the
#' number of points within the given radius.
#' 
#' @param x, y Equal-length numeric vectors of coordinates
#' @param radius Size of the neighbourhood
#' 
#' @return Numeric vector the same length as `x` and `y` with the number of
#' points within the specified radius of each point (counting the point itself,
#' so always >=1).
#'
#' @export
#'
#' @examples
#' local_count(AZ_A1020_BLM$x, AZ_A1020_BLM$y, radius = 2)
local_count <- function(x, y, radius) {
  stopifnot(is.numeric(radius))
  rowSums(dist_matrix(x, y) <= radius)
}

#' Distance matrix for two vectors of coordinates
#'
#' @return A matrix, see [stats::as.matrix.dist()].
#' 
#' @param x, y Equal-length numeric vectors of coordinates
#' @param method Passed to [stats::dist()]
#'
#' @noRd
#' @keywords internal
dist_matrix <- function(x, y, method = "euclidean") {
  coords <- coord_matrix(x, y)
  as.matrix(stats::dist(coords, method))
}

#' Convert two vectors of coordinates into a 2-column matrix
#'
#' @param x, y Equal-length numeric vectors of coordinates
#'
#' @noRd
#' @keywords internal
coord_matrix <- function(x, y) {
  stopifnot(is.numeric(x) & is.numeric(y))
  stopifnot(length(x) == length(y))
  matrix(c(x, y), ncol = 2)
}
