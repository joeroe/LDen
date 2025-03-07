

test_that("distance_matrix returns a matrix of appropriate size", {
  mat <- distance_matrix(test_locs)

  expect_equal(nrow(mat), nrow(test_locs))
  expect_equal(ncol(mat), nrow(test_locs))
})
