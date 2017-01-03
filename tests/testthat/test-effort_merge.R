library(metagear)
context("Does effort_merge function ok?")


test_that("merge files from screening efforts", {
  data(example_references_metagear)
  theTeam <- c("Christina", "Luc")
  effort_distribute(example_references_metagear, initialize = TRUE, 
                    reviewers = theTeam, save_split = TRUE)
  expect_true(file.exists("effort_Luc.csv"))
  expect_true(file.exists("effort_Christina.csv"))
  merged <- effort_merge()
  expect_true(all(unique(merged$REVIEWERS) == c("Christina", "Luc"))) 
  file.remove(c("effort_Luc.csv", "effort_Christina.csv"))
})

test_that("merge files from dual screening efforts", {
  data(example_references_metagear)
  theTeam <- c("Christina", "Luc", "Patsy", "Noelle")
  fak <- effort_distribute(example_references_metagear, initialize = TRUE, 
                    reviewers = theTeam, dual = TRUE, save_split = TRUE)
  expect_true(file.exists("effort_Luc.csv"))
  expect_true(file.exists("effort_Christina.csv"))
  expect_true(file.exists("effort_Patsy.csv"))
  expect_true(file.exists("effort_Noelle.csv"))
  merged <- effort_merge(dual = TRUE)
  expect_true(all(unique(merged$REVIEWERS_A) == c("Christina", "Patsy"))) 
  expect_true(all(unique(merged$REVIEWERS_B) == c("Luc", "Noelle"))) 
  file.remove(c("effort_Luc.csv", "effort_Christina.csv",
                "effort_Patsy.csv", "effort_Noelle.csv"))
  
})