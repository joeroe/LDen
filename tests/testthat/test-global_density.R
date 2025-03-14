source(file = "../testdata.R")

test_that("global density is calculated correctly", {
  expect_equal(
    global_density(test_locs$x, test_locs$y, area = 10),
    nrow(test_locs) / 10
  )
})
