library(metagear)
context("Is the random_r function ok?")


test_that("returns specified number of randoms", {

  aDataTable <- random_r(30)
  expect_equal(dim(aDataTable), c(30, 3))
})