# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(LDen)

test_locs <- data.frame(
  x = c(1, 1, 2, 3, 8, 9, 9, 10, 6, 6, 5, 7, 12, 13, 14, 15),
  y = c(1, 2, 1, 3, 2, 1, 3, 2, 10, 11, 12, 12, 8, 7, 9, 7),
  type = c(0, 0, 0, 0, 2, 0, 2, 0, 10, 11, 12, 13, 4, 2, 2, 4)
)


test_check("LDen")
