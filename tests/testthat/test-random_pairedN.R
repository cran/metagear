library(metagear)
context("Is the random_pairedN function ok?")


test_that("returns specified number of randoms", {

  aDataTable <- random_pairedN(30)
  expect_equal(dim(aDataTable), c(30,2))
})