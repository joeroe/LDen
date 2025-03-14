#' Global point density
#'
#' Calculates the 'global' density of a vector of point coordinates in relation 
#' to the total size of the window or study region (e.g. the site or trench).
#'
#' @param x,y Equal-length numeric vectors of coordinates
#' @param area Total area of the window
#'
#' @return Number of points divided by area
#' @export
#'
#' @examples
#' global_density(AZ_A1020_BLM$x, AZ_A1020_BLM$y, area = 2409)
global_density <- function(x, y, area = NULL) {
  stopifnot(length(x) == length(y))
  length(x) / area
}
