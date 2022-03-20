library(testthat,lib.loc="C:/Users/Max/Documents/R/win-library/4.0")
source('R/subroutines.R')

###################
# ALL SUBROUTINES #
test_that("All the subroutine functions exist", {
  expect_true(exists('PrepareMemParam'))
  expect_true(exists('generateRandomMultivarRegreParam'))
  expect_true(exists('createMatrixonW'))
  expect_true(exists('completeGmatrix'))
  expect_true(exists('isSymmetric'))
  expect_true(exists('sweep'))
  expect_true(exists('generateMissingvalue'))
  expect_true(exists('MIEC'))
})

###################
# PrepareMemParam #
