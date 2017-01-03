library(metagear)
context("Is the random_N function ok?")


test_that("returns specified number of randoms", {

  aDataTable <- random_N(30)
  expect_equal(length(aDataTable), 30)
})