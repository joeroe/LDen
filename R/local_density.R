#' Calculate Local Density
#'
#' `local_density()` is a function that calls the [local_counts()] function,
#' then calculates the area of the neighborhood defined by the specified
#' radius. The *local density* is the count divided by the area of the
#' neighborhood.
#'
#' @seealso [local_counts()]
#'
#'
#' @inheritParams local_counts
#'
#' @return a data.frame that includes the point location and type of
#' each point, as well as the density of points of each type within
#' the specified radius of each point.
#'
#' @export
#'
#' @examples
local_density <- function(location_data, x = 1, y = 2, radius) {
  counts <- local_counts(location_data, radius = radius)
  local_densities <- counts

  # prep a vector of columns needed in the following operations:
  count_cols <- colnames(local_densities)
  count_cols <- count_cols[grep("count", colnames(local_densities))]
  count_cols <- count_cols[-which(count_cols == "count_total")]

  # this part reduces the same-type counts and the total counts for
  # each point by 1 so that points don't count in calculating their
  # own local densities:

  # remove 1 from each total
  local_densities$count_total <- local_densities$count_total - 1

  # loop over all columns of counts
  for (col in count_cols) {
    # extract the type from the column name
    this_type <- gsub("count_", "", col)
    # get the index of all affected rows where the count should be reduced
    # TODO This may lead to problems if $type is a factor
    index <- which(local_densities$type == this_type)
    # subtract one in the rows that represent the same type as the current one
    local_densities[index, col] <- local_densities[index, col] - 1
  }

  # this part calculates the densities based on the area and the counts
  area <- pi * radius^2

  # re-add the count_total column for easier looping
  count_cols <- c(count_cols, "count_total")

  for (col in count_cols) {
    local_densities[, col] <- local_densities[, col] / area
  }
  # and rename:
  colnames(local_densities) <- gsub("count_", "density_", colnames(local_densities))


  return(local_densities)
}
