#' Local point counts and density
#'
#' For a vector of points specified by x and y coordinates, `local_count()`
#' calculates the number of points within the given radius. `local_density()`
#' returns the density of points, i.e. the count divided by the area of a circle
#' with the given radius.
#'
#' `local_count2()` and `local_density2()` are two-sample versions of the above,
#' calculating the count of density of points in y within the specified radius
#' of points in x.
#'
#' @param x,y Equal-length numeric vectors of coordinates
#' @param x1,x2,y1,y2 Equal-length numeric vectors of coordinates (for two-sample versions)
#' @param radius Size of the neighbourhood
#'
#' @return Numeric vector the same length as `x` with the number (counting the
#' point itself, so always >=1) or density points within the specified radius of
#' each point.
#'
#' @export
#'
#' @examples
#' local_count(AZ_A1020_BLM$x, AZ_A1020_BLM$y, radius = 2)
#' local_density(AZ_A1020_BLM$x, AZ_A1020_BLM$y, radius = 2)
local_count <- function(x, y, radius) {
  stopifnot(is.numeric(radius))
  rowSums(dist_matrix(x, y) <= radius)
}

#' @rdname local_count
#' @export
local_density <- function(x, y, radius) {
  local_count(x, y, radius) / (pi * radius^2)
}

#' @rdname local_count
#' @export
local_count2 <- function(x1, y1, x2, y2, radius) {
  coords1 <- coord_matrix(x1, y1)
  coords2 <- coord_matrix(x2, y2)
  dist_matrix <- as.matrix(proxy::dist(coords1, coords2, method = "euclidean"))
  rowSums(dist_matrix <= radius)
}

#' @rdname local_count
#' @export
local_density2 <- function(x1, y1, x2, y2, radius) {
  local_count2(x1, y1, x2, y2, radius) / (pi * radius^2)
}

# Unexported functions ----------------------------------------------------

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

dist_matrix2 <- function(x, y, method = "euclidean") {

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
