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
#' @importFrom dplyr filter
#'
#' @return
#' @export
#'
#' @examples
glb_density <- function(location_data, site_area) {
  type_list <- unique(location_data$type)
  type_list <- sort(type_list)
  calc_density <- function(artifact_type) {
    temp <- dplyr::filter(location_data, type == artifact_type)
    nrow(temp) / site_area
  }
  
  global_density <- sapply(type_list, FUN = calc_density)
  
  global_density[length(type_list) + 1] <- nrow(location_data) / site_area
  
  global_density <-  as.matrix(global_density)
  row.names(global_density) <- c(type_list, "total")
  
  return(global_density)
}

