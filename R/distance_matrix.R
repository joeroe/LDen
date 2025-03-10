#' Distance Matrix
#'
#' The distance_matrix function finds the difference from each point to
#' every other point, and returns a rectangular matrix (which means each
#' distance is in the matrix twice, once from point A to point B,
#' and again from point B to point A)
#'
#' @param location_data expects a data.frame with three columns:
#' \describe{
#'   \item{x}{Easting / x-coordinate of location}
#'   \item{y}{Northing / y-coordinate of location}
#'   \item{type}{categorical variable for type of evidence at location}
#' }
#' @param x Index of column containing the x-coordinate
#' @param y Index of column containing the y-coordinate
#'
#' @return A square matrix containing the distances between all points.
#'
#' @export
#'
#' @examples
distance_matrix <- function(location_data, x = 1, y = 2){
  stopifnot(all(is.numeric(location_data[,y]), is.numeric(location_data[,x])))

  as.matrix(stats::dist(x = location_data[,c(x,y)], method = "euclidean"))
}
