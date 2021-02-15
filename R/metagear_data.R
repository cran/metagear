#' A collection of bibliographic references
#'
#' An example dataset containing 11 journal references.  The variables are 
#' described below.
#'
#' \itemize{
#'   \item AUTHORS. Authors of the journal article
#'   \item YEAR. Publication year
#'   \item TITLE. Article title
#'   \item JOURNAL. Journal name
#'   \item VOLUME. Journal volume number
#'   \item LPAGES. Lower page number
#'   \item UPAGES. Upper page number
#'   \item DOI. Digital object identifier (DOI) of journal article
#'   \item ABSTRACT. Full text of the journal article abstract
#' }
#'
#' @docType data
#' @keywords datasets
#' @name example_references_metagear
#' @usage data(example_references_metagear)
#' @format A data frame with 12 rows and 9 variables.
NULL

#' An example image of a scatterplot figure
#'
#' A jpg image of a scatterplot from Figure 2 of Kam, M., Cohen-Gross, S., 
#'    Khokhlova, I.S., Degen, A.A. and Geffen, E. 2003. Average daily metabolic 
#'    rate, reproduction and energy allocation during lactation in the Sundevall
#'    Jird Meriones crassus. Functional Ecology 17:496-503.
#'
#' @docType data
#' @keywords datasets
#' @name Kam_et_al_2003_Fig2.jpg
#' @format A raw jpg-formated image
#' @note \strong{How to use}\cr\cr 
#'    \code{readImage(system.file("images", "Kam_et_al_2003_Fig2.jpg", package = "metagear"))}

NULL

#' An example image of a bar plot figure
#'
#' A jpg image of a bar plot from Figure 4 of Kortum, P., and Acymyan, C.Z. 
#'    2013. How low can you go? Is the System Usability Scale range restricted? 
#'    Journal of Usability Studies 9:14-24.
#'
#' @docType data
#' @keywords datasets
#' @name Kortum_and_Acymyan_2013_Fig4.jpg
#' @format A raw jpg-formated image
#' @note \strong{How to use}\cr\cr 
#'    \code{readImage(system.file("images", "Kortum_and_Acymyan_2013_Fig4.jpg", package = "metagear"))}
NULL