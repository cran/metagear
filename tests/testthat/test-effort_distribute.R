library(metagear)
context("Does effort_distribute function ok?")


test_that("distribute effort among single Reviewer without options", {
  data(example_references_metagear)
  theTeam <- c("Christina")
  aDataFrame <- effort_distribute(example_references_metagear, 
	                              initialize = TRUE, 
	                              reviewers = theTeam)
  expect_equal(dim(aDataFrame), c(11, 12))
  expect_equal(as.vector(aDataFrame$REVIEWERS), rep("Christina", 11))
})

test_that("distribute effort among two Reviewers without dual", {
	data(example_references_metagear)
	theTeam <- c("Christina", "Luc")
	aDataFrame <- effort_distribute(example_references_metagear, 
	                                initialize = TRUE, 
	                                reviewers = theTeam,
	                                effort = c(0, 100))
	expect_equal(dim(aDataFrame), c(11, 12))
	expect_equal(as.vector(aDataFrame$REVIEWERS), rep("Luc", 11))
	
	aDataFrame <- effort_distribute(example_references_metagear, 
	                                initialize = TRUE, 
	                                reviewers = theTeam,
	                                effort = c(100, 0))
	expect_equal(as.vector(aDataFrame$REVIEWERS), rep("Christina", 11))
	
	aDataFrame <- effort_distribute(example_references_metagear, 
	                                initialize = TRUE, 
	                                reviewers = theTeam,
	                                effort = c(36, 64))
	expect_equal(sum(aDataFrame$REVIEWERS == "Luc"), 7)
	expect_equal(sum(aDataFrame$REVIEWERS == "Christina"), 4)
})


test_that("distribute effort among 4 Reviewers with dual screening design", {
  data(example_references_metagear)
  theTeam <- c("Christina", "Luc", "Chachi")
  expect_error(effort_distribute(example_references_metagear, 
                                 initialize = TRUE, 
                                 dual = TRUE, 
                                 reviewers = theTeam))
  
  theTeam <- c("Christina", "Luc", "Chachi", "Noelle")
  aDataFrame <- effort_distribute(example_references_metagear, 
                                  initialize = TRUE, 
                                  dual = TRUE, 
                                  reviewers = theTeam)
  expect_equal(dim(aDataFrame), c(11, 14))
	expect_equal(sum(aDataFrame$REVIEWERS_A == "Christina"), 6)
	expect_equal(sum(aDataFrame$REVIEWERS_A == "Chachi"), 5)
	expect_equal(sum(aDataFrame$REVIEWERS_B == "Luc"), 6)
	expect_equal(sum(aDataFrame$REVIEWERS_B == "Noelle"), 5)
})    
    

test_that("file saving in distribute effort among 4 reviewers with dual screening design", {
  data(example_references_metagear)
  theTeam <- c("Christina", "Luc", "Chachi", "Noelle")
  aDataFrame <- effort_distribute(example_references_metagear, 
                                  initialize = TRUE, 
                                  dual = FALSE,
                                  save_split = TRUE,
                                  reviewers = theTeam)
	expect_true(file.exists("effort_Luc.csv"))
	expect_true(file.exists("effort_Christina.csv"))
	expect_true(file.exists("effort_Chachi.csv"))
	expect_true(file.exists("effort_Noelle.csv"))
	file.remove(c("effort_Luc.csv", "effort_Christina.csv",
	              "effort_Chachi.csv", "effort_Noelle.csv"))
}) 