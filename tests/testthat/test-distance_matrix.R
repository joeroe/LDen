source(file = "../testdata.R")

test_that("distance_matrix returns a matrix of appropriate size", {
  mat <- distance_matrix(test_locs)

  expect_equal(nrow(mat), nrow(test_locs))
  expect_equal(ncol(mat), nrow(test_locs))
})


test_that("distance_matrix preserves rownames",{
  rownames(test_locs) <- paste0("place_", seq_len(nrow(test_locs)))
  mat <- distance_matrix(test_locs)

  expect_identical(rownames(mat), rownames(test_locs))
  expect_identical(colnames(mat), rownames(test_locs))
})

test_that("distance_matrix stops for invalid x/y",{
  test_locs$x <- as.character(test_locs$x)
  expect_error(distance_matrix(test_locs), "numeric")
  test_locs$y <- as.character(test_locs$y)
  test_locs$x <- as.numeric(test_locs$x)
  expect_error(distance_matrix(test_locs), "numeric")
})
