#' Calculate Local Density
#'
#' `local_density()` is a function that calls the local_count function,
#' then calculates the area of the neighborhood defined by the specified
#' radius. The local_density is the count divided by the area of the
#' neighborhood. The function outputs a dataframe that includes the point
#' location and type of each point, as well as the density of points of
#' each type within the specified radius of each point.
#'
#' @seealso [local_count()]
#'
#'
#' @inheritParams distance_matrix
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
local_density <- function(location_data, radius) {
  counts <- local_counts(location_data, radius)
  local_densities <- counts
  type_list <- unique(local_densities$type)
  type_list <- sort(type_list)

  # reduce the same-type counts and the total counts for
  # each point by 1 so that points don't count in calculating their
  # own local densities
  for (i in 1:nrow(local_densities)) {
    local_densities[i,ncol(local_densities)] <- local_densities[i,ncol(local_densities)] - 1
    for (j in 1:length(type_list))
      if (local_densities$type[i] == type_list[j]) {
        local_densities[i,j + 4] <- local_densities[i,j + 4] - 1
      }
  }

  area <- pi * radius^2
  col_list <- colnames(local_densities)
  for (i in 5:ncol(local_densities)) {
    col_list[i] <- substring(col_list[i],7)
    col_list[i] <- paste0("density_", col_list[i])
    for (j in 1:nrow(local_densities)) {
      local_densities[j,i] <- local_densities[j,i] / area
    }
  }
  colnames(local_densities) <- col_list

  return(local_densities)

}
