testthat::test_that("multiplication works", {
  expect_equal(check_gene("TraesCS1A03G0010400", "size"), TRUE)
})

library(testthat)
testthat::test_that("multiplication works", {
  expect_equal(check_gene("TraesCS1A03G0010400", "name"), TRUE)
})
