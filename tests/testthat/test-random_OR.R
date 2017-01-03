library(metagear)
context("Is the random_OR function ok?")


test_that("returns specified number of randoms", {

  aDataTable <- random_OR(5, 0.3, 100, 0.1, 60)
  expect_equal(dim(aDataTable), c(5, 2))
})