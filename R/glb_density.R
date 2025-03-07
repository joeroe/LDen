#' Calculate Global Density
#'
#' `glb_density()` calculates the global density of points of each type
#' (the number of points of the type divided by the total area)
#' and returns a vector of the global densities that gets used by the
#' [lda()] function to calculate the local density coefficient
#'
#' @inheritParams distance_matrix
#' @param site_area
#'
#' @return
#' @export
#'
#' @examples
glb_density <- function(location_data, site_area) {
  type_list <- unique(location_data$type)
  type_list <- sort(type_list)
  global_density <- matrix(nrow = length(type_list) + 1)
  for (i in 1:length(type_list)) {
    temp <- dplyr::filter(location_data, type == type_list[i])
    global_density[i] <- nrow(temp) / site_area
  }
  global_density[length(type_list) + 1] <- nrow(location_data) / site_area
  row.names(global_density) <- c(type_list, "total")

  return(global_density)
}
