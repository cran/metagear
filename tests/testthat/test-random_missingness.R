library(metagear)
context("Is the random_missingness function ok?")


test_that("returns specified number of randoms", {
  
  data(example_references_metagear)
  aDataFrame <- random_missingness(example_references_metagear, "YEAR", 
								   percentMissing = 40)
  expect_true(anyNA(aDataFrame$YEAR))
  
  aDataFrame <- random_missingness(example_references_metagear, c("YEAR", "JOURNAL"), 
								   percentMissing = 40)
  expect_true(anyNA(aDataFrame$YEAR))
  expect_true(anyNA(aDataFrame$JOURNAL))
  
  aDataFrame <- random_missingness(example_references_metagear, "YEAR", 
								   percentMissing = 0)
  expect_false(anyNA(aDataFrame$YEAR))
  
  aDataFrame <- random_missingness(example_references_metagear, "YEAR", 
								   percentMissing = 100)
  expect_true(all(is.na(aDataFrame$YEAR)))
  
})