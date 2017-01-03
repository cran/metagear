library(metagear)
context("Does effort_initialize function ok?")

test_that("initialize bibliographic dataset", {
  data(example_references_metagear)
  aDataFrame <- effort_initialize(example_references_metagear)
  expect_equal(dim(aDataFrame), c(11, 12))
  expect_true("STUDY_ID" %in% colnames(aDataFrame))
  expect_equal(sum(aDataFrame$STUDY_ID), 66)
  expect_true("REVIEWERS" %in% colnames(aDataFrame))
  expect_true(all(is.na(aDataFrame$REVIEWERS)))
  expect_true("INCLUDE" %in% colnames(aDataFrame))
})

test_that("initialize dual screening bibliographic dataset", {
  data(example_references_metagear)
  aDataFrame <- effort_initialize(example_references_metagear,
								  dual = TRUE)
  expect_equal(dim(aDataFrame), c(11, 14))
  expect_true("REVIEWERS_A" %in% colnames(aDataFrame))
  expect_true("INCLUDE_A" %in% colnames(aDataFrame))
  expect_true("REVIEWERS_B" %in% colnames(aDataFrame))
  expect_true("INCLUDE_B" %in% colnames(aDataFrame))
})