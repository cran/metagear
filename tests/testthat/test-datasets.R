library(metagear)
context("Are prepackaged datasets ok?")

test_that("example_references_metagear bibliographic table is properly loaded", {
	data(example_references_metagear)
	expect_is(example_references_metagear, "data.frame")
	expect_equal(dim(example_references_metagear), c(11, 9))
})

# NOTE: skip below tests until I figure out how to include tests with 
# a bioconductor package

#test_that("Kam_et_al_2003_Fig2 EBImage image is properly loaded", {
#	data(Kam_et_al_2003_Fig2)
#	expect_is(Kam_et_al_2003_Fig2, "Image")
#	expect_equal(dim(Kam_et_al_2003_Fig2), c(803, 566, 3))
#})

#test_that("Kortum_and_Acymyan_2013_Fig4 EBImage image is properly loaded", {
#	data(Kortum_and_Acymyan_2013_Fig4)
#	expect_is(Kortum_and_Acymyan_2013_Fig4, "Image")
#	expect_equal(dim(Kortum_and_Acymyan_2013_Fig4), c(784, 523, 3))
#})