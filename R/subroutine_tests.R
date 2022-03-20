#!/usr/bin/env C:/Program Files/R/R-4.0.2/bin/Rscript.exe

library(testthat,
        lib.loc=c("C:/Users/Max/Documents/R/win-library/4.0",
                  "C:/Program Files/R/R-4.0.2/library"))
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
