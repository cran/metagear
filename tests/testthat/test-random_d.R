library(metagear)
context("Is the random_d function ok?")


test_that("returns specified number of randoms", {

  aDataTable <- random_d(30, 10, 0.5, 15, 5, 0.5, 15)
  expect_equal(dim(aDataTable), c(30, 2))
})