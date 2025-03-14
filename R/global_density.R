#' Global point density
#'
#' Calculates the 'global' density of a vector of point coordinates in relation 
#' to the total size of the window or study region (e.g. the site or trench).
#'
#' @param x,y Equal-length numeric vectors of coordinates
#' @param area Total area of the window. If `NULL`, inferred as the area of the
#'   bounding box of the coordinates specified by `x` and `y`.
#'
#' @return Number of points divided by area
#' @export
#'
#' @examples
#' global_density(AZ_A1020_BLM$x, AZ_A1020_BLM$y, area = 2409)
#' global_density(AZ_A1020_BLM$x, AZ_A1020_BLM$y)
global_density <- function(x, y, area = NULL) {
  stopifnot(length(x) == length(y))
  if (is.null(area)) area <- area_bbox(x, y) # TODO: message with area?
  length(x) / area
}

#' Calculate area of the bounding box of a set of coordinates
#'
#' @noRd
#' @keywords internal
area_bbox <- function(x, y) {
  stopifnot(is.numeric(x) & is.numeric(y))
  stopifnot(all(!is.na(c(x, y)))) # TODO: or set na.rm below?
  (max(x) - min(x)) * (max(y) - min(y))
}
