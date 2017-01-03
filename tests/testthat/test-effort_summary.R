library(metagear)
context("Does effort_summary function ok?")

test_that("summary of non-dual screening effort", {
  data(example_references_metagear)
  theTeam <- c("Christina", "Luc")
  aDataFrame <- effort_distribute(example_references_metagear, initialize = TRUE, 
                                  reviewers = theTeam)
  aDataFrame["INCLUDE"] <- c("YES", "NO", "NO", "YES", "MAYBE", "YES", "MAYBE", 
                             "NO", "NO", "NO", "YES")
  aSummary <- effort_summary(aDataFrame)
  expect_equal(dim(aSummary), c(3, 5))
  expect_equal(aSummary$TOTAL, c(6, 5, 11))
})

# test_that("summary of dual screening effort", {
#  data(example_references_metagear)
#  theTeam <- c("Christina", "Luc", "Chachi", "Noelle")
#  aDataFrame <- effort_distribute(example_references_metagear, 
#                                  initialize = TRUE, 
#                                   dual = TRUE, 
#                                   reviewers = theTeam)
#   aDataFrame["INCLUDE_A"] <- c("YES", "NO", "NO", "YES", "MAYBE", "YES", "MAYBE", 
#                                "NO", "NO", "NO", "YES")
#   aDataFrame["INCLUDE_B"] <- c("YES", "YES", "NO", "YES", "MAYBE", "YES", "MAYBE", 
#                                "NO", "NO", "NO", "YES")
#   aSummary <- effort_summary(aDataFrame, dual = TRUE)
#   expect_equal(dim(aSummary), c(3, 15))
#   expect_match(as.character(aSummary[1, 15]), "0.714285714285714")
# })