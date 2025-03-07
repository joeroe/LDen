#' Title
#'
#' Local_counts is a function to find the counts of points of different
#' types within the local neighborhood of each point -- points are
#' included in the counts of their own neighborhood.
#' The output is a dataframe that includes the point location and
#' type of each point, as well as counts (by type) of points within the
#' specified radius of each point.
#'
#' @inheritParams distance_matrix
#' @param radius *numeric*. The radius of what
#'
#'
#' @return a data.frame that includes the point location and
#' type of each point, as well as counts (by type) of points within the
#' specified radius of each point
#'
#' @export
#'
#' @examples
#'
#' locations <- LDen::AZ_A1020_BLM
#' local_counts <- local_counts(locations, radius = 2)
#'
local_counts <- function(location_data, radius) {

  stopifnot(is.numeric(radius))

  distance <- LDen::distance_matrix(location_data = location_data)
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

  # find the total count of neighboring artifacts
  total_neighbour_count <- apply(distance, 1, function(one_row) {
    sum(one_row <= radius)
  })

  output <- cbind(output, neighbor_counts_per_type)
  colnames(output)[(ncol(output)-num_types+1):ncol(output)] <- paste0("count_", as.character(type_list))
  output$count_total <- total_neighbour_count

  return(output)
}
