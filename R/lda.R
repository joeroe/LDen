#' LDA: Local Density ...?
#'
#' `lda()` calculates the local density coefficient within and between
#' all the types in the dataset. The local density coefficient from
#' Type A to Type B is the mean density of points of Type B within
#' the specified radius of points of Type A, divided by the global
#' density of points of Type B (i.e., the number of points of
#' Type B divided by the total area)
#'
#' @inheritParams local_counts
#' @inheritParams glb_density
#'
#' @return A data.frame containing the local density coefficients.
#'
#' @export
#'
#' @examples
lda <- function(location_data, x = 1, y = 2, radius, site_area) {
  for (n in 1:length(radius)) {
    densities <- local_density(location_data, radius = radius[n])
    type_list <- unique(location_data$type)
    type_list <- sort(type_list)
    global_density <- glb_density(location_data, site_area = site_area)
    lda_matrix <- matrix(nrow = length(type_list) + 1, ncol = length(type_list) + 2)
    name_list <- c(as.character(type_list), "total")
    name_list_col <- c(paste0("ldc_", name_list), "radius")
    colnames(lda_matrix) <- name_list_col

    for (i in 1:(length(type_list) + 1)) {
      if (i <= length(type_list)){
        current_type <- densities[densities$type == type_list[i], ]
        for (j in 1:(length(type_list) + 1)) {
          lda_matrix[i,j] <- round(mean(current_type[,j + 4]) / global_density[j], 2)
        }
      }
      else {
        for (j in 1:(length(type_list) + 1)) {
          lda_matrix[(length(type_list) + 1),j] <- mean(densities[,j + 4]) / global_density[j]
        }
      }

    }
    lda_matrix <- as.data.frame(round(lda_matrix, 2))
    type <- name_list
    lda_matrix <- cbind(type, lda_matrix)
    lda_matrix$radius <- radius[n]
    if (n == 1) {
      lda_out <- lda_matrix
    }
    else {
      lda_out <- rbind(lda_out, lda_matrix)
    }
  }

  return(lda_out)
}
