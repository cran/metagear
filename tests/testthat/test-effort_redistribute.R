library(metagear)
context("Does effort_redistribute function ok?")


test_that("distribute effort among 2 then redistribute among 3 reviewers", {
  data(example_references_metagear)
  theTeam <- c("Christina", "Luc")
  aDataFrame <- effort_distribute(example_references_metagear, 
                                  reviewers = theTeam, effort = c(20, 80))
  # move 50% of Luc's work to new reviewer Patsy
  aDataFrame <- effort_redistribute(aDataFrame, 
                                    reviewer = "Luc",
                                    remove_effort = 50, 
                                    reviewers = c("Luc", "Patsy"))
  testTable <- table(aDataFrame$REVIEWERS)
  expect_equal(as.numeric(testTable["Christina"]), 2)
  expect_equal(as.numeric(testTable["Luc"]), 5)
  expect_equal(as.numeric(testTable["Patsy"]), 4)
})
