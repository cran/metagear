#' Research synthesis tools to facilitate systematic reviews, data extraction, 
#' and meta-analysis.
#'
#' \pkg{metagear} is a comprehensive, multifunctional toolbox with capabilities 
#' aimed to cover much of the research synthesis taxonomy: from applying a 
#' systematic review approach to objectively assemble and screen the literature, 
#' to extracting data from studies, and to finally summarize and analyze these 
#' data with the statistics of meta-analysis.  More information about 
#' \pkg{metagear} can be found at \url{http://lajeunesse.myweb.usf.edu/}.
#'
#' @details \strong{What to cite?}\cr\cr Lajeunesse, M.J. (2016) Facilitating 
#' systematic reviews, data extraction and meta-analysis with the metagear 
#' package for R. \emph{Methods in Ecology and Evolution} 7: 323-330. [ download 
#' \href{http://lajeunesse.myweb.usf.edu/papers/Lajeunesse_2016_Methods_in_Ecology_and_Evolution.pdf}{here} ]
#' \cr\cr \strong{Installation and Dependencies.}\cr\cr \pkg{metagear} has one 
#' external dependency that need to be installed and loaded prior to use in R.
#' This is the EBImage R package (Pau et al. 2010) available only from the 
#' Bioconductor repository: \url{https://www.bioconductor.org/}. 
#' \cr\cr To properly install \pkg{metagear}, start with the following
#' R script that loads the Bioconductor resources needed to install the EBImage 
#' (also accept all of its dependencies): \cr\cr
#' \code{install.packages("BiocManager");} \cr \code{BiocManager::install("EBImage"))} 
#' \cr \code{library(metagear)} \cr\cr Finally for Mac OS users, installation 
#' is sometimes not straighforward as the abstract_screener() requires the
#' Tcl/Tk GUI toolkit to be installed. You can get this toolkit by making sure
#' the latest X11 application (xQuartz) is installed from here: 
#' \url{https://www.xquartz.org/}.
#'
#' @references Pau, G., Fuchs, F., Sklyar, O., Boutros, M. and Huber, W. (2010)
#' EBImage: an R package for image processing with applications to cellular
#' phenotypes. Bioinformatics 26: 979-981.
#'
#' @name metagear-package
#' @docType package
#' @author Marc J. Lajeunesse (University of South Florida, Tampa USA)

NULL