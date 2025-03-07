source(file = "../testdata.R")

# copied from the file "LDEN.TXT"
# Points 16    Area 154.00    Radius 1.00
res <- data.frame(
  ldc_0 = c(5.45, 0, 0, 0, 0, 0, 0),
  ldc_2 = c(0, 0, 0, 0, 0, 0, 0),
  ldc_4 = c(0, 0, 0, 0, 0, 0, 0),
  ldc_10 = c(0, 0, 0, 0, 49.02, 0, 0),
  ldc_11 = c(0, 0, 0, 49.02, 0, 0, 0),
  ldc_12 = c(0, 0, 0, 0, 0, 0, 0),
  ldc_13 = c(0, 0, 0, 0, 0, 0, 0)
)
rownames(res) <- c(0, 2, 4, 10, 11, 12, 13)

test_that("result is equal to 'old' output with radius 1", {
  local_density <- lda(test_locs, 1, 154)
  rownames(local_density) <- local_density$type
  local_density <- local_density[rownames(res), ]
  local_density <- local_density[, colnames(res)]

  expect_equal(res, local_density)
})

# copied from the file "LDEN.TXT",
# Points 16    Area 154.00    Radius 2.00
res <- data.frame(
  ldc_0 = c(2.72, 2.04, 0.00, 0.00, 0.00, 0.00, 0.00),
  ldc_2 = c(2.04, 1.53, 3.06, 0.00, 0.00, 0.00, 0.00),
  ldc_4 = c(0.00, 3.06, 0.00, 0.00, 0.00, 0.00, 0.00),
  ldc_10 = c(0.00, 0.00, 0.00, 0.00, 12.25, 0.00, 0.00),
  ldc_11 = c(0.00, 0.00, 0.00, 12.25, 0.00, 12.25, 12.25),
  ldc_12 = c(0.00, 0.00, 0.00, 0.00, 12.25, 0.00, 12.25),
  ldc_13 = c(0.00, 0.00, 0.00, 0.00, 12.25, 12.25, 0.00)
)
rownames(res) <- c(0, 2, 4, 10, 11, 12, 13)

# copied from the file "LDEN.TXT",
# Points 16    Area 154.00    Radius 5.00
test_that("result is equal to 'old' output with radius 2", {
  local_density <- lda(test_locs, 2, 154)
  rownames(local_density) <- local_density$type
  local_density <- local_density[rownames(res), ]
  local_density <- local_density[, colnames(res)]

  expect_equal(res, local_density)
})

res <- data.frame(
  ldc_0 = c(0.76, 0.33, 0.00, 0.00, 0.00, 0.00, 0.00),
  ldc_2 = c(0.33, 0.49, 0.98, 0.00, 0.00, 0.00, 0.00),
  ldc_4 = c(0.00, 0.98, 0.98, 0.00, 0.00, 0.00, 0.00),
  ldc_10 = c(0.00, 0.00, 0.00, 0.00, 1.96, 1.96, 1.96),
  ldc_11 = c(0.00, 0.00, 0.00, 1.96, 0.00, 1.96, 1.96),
  ldc_12 = c(0.00, 0.00, 0.00, 1.96, 1.96, 0.00, 1.96),
  ldc_13 = c(0.00, 0.00, 0.00, 1.96, 1.96, 1.96, 0.00)
)
rownames(res) <- c(0, 2, 4, 10, 11, 12, 13)

test_that("result is equal to 'old' output with radius 5", {
  local_density <- lda(test_locs, 5, 154)
  rownames(local_density) <- local_density$type
  local_density <- local_density[rownames(res), ]
  local_density <- local_density[, colnames(res)]

  expect_equal(res, local_density)
})
