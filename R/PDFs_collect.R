#' Attempts to download PDFs from multiple DOI links.
#'
#' Tries to download a collection of PDF files using multiple digital object 
#' identifier (DOI) links.  Updates a data frame with the success of these 
#' downloads.  The function is a wrapper for \code{\link{PDF_download}}.  NOTE: 
#' A single DOI may generate multiple PDF files.  \strong{If running downloader 
#' in Windows, having \code{"WindowsProxy = TRUE"} will significantly improve 
#' download success.}  
#'
#' @param aDataFrame A data frame containing a column of DOIs and a column of
#'    individual file names for each downloaded PDF.
#' @param DOIcolumn The label of the column containing all the DOI links.
#' @param FileNamecolumn The label of the column containing all the strings 
#'    that will be used to rename the downloaded files.
#' @param directory A string of the location (directory) were downloaded PDF
#'    files are to be saved.  NOTE: helps to have this directory created before
#'    initializing the \code{PDFs_collect} function.
#' @param randomize When \code{TRUE} will attempt to download PDFs in a random
#'    order.  This may be necessary to ensure that host websites do not have
#'    their HTML and files repeatedly accessed.
#' @param seed An integer used to enforce repeatability when randomly 
#'    downloading PDFs. 
#' @param buffer When \code{TRUE} will randomly delay the downloads by a few
#'    seconds (with a mean 4 seconds and a range of 1 to 20 seconds). Another
#'    strategy to avoid quickly and repeatedly accessing host websites.
#' @param validatePDF When \code{TRUE} will only save to files that are valid
#'    PDF documents.  When \code{FALSE} will save all candidate files, even if
#'    they are not valid PDF formats.
#' @param quiet When \code{FALSE} does not print to console individual 
#'    download progress and summary.
#' @param showSummary When \code{FALSE} does not print overall summary of download
#'    successes and failures.
#' @param WindowsProxy When \code{TRUE} significantly improves download success 
#'    for computers running Windows; when \code{FALSE} on a Windows based 
#'    computer, you may only be able to download 30 to 50 PDFs at a time before 
#'    a connection error occurs and halts all downloads (e.g., 
#'    \code{InternetOpenUrl failed} error).
#'
#' @return The data frame with new column containing download-outcome successes.
#'
#' @examples \dontrun{
#'
#' data(example_references_metagear)
#' someRefs <- effort_initialize(example_references_metagear)  
#' dir.create("metagear_downloads")      
#' PDFs_collect(aDataFrame = someRefs, DOIcolumn = "DOI", 
#'              FileNamecolumn = "STUDY_ID", directory = "metagear_downloads",
#'				WindowsProxy = TRUE)
#' }
#'
#' @seealso \code{\link{PDF_download}}
#'
#' @importFrom utils download.file 
#' @export PDFs_collect

PDFs_collect <- function(aDataFrame, 
                         DOIcolumn, 
                         FileNamecolumn, 
                         directory = getwd(), 
                         randomize = FALSE,
                         seed = NULL,
                         buffer = FALSE,
                         validatePDF = TRUE, 
                         quiet = FALSE, 
                         showSummary = TRUE,
                         WindowsProxy = FALSE) {


  # set randomizer seed for buffer repeatability                       
  if(!is.null(seed)) set.seed(seed)
  
  # randomly shuffle order of DOIs 
  if(randomize == TRUE) {
    DOIcolumn <- sample(aDataFrame[, DOIcolumn])
  } else {
    DOIcolumn <- aDataFrame[, DOIcolumn]
  }
 
  totalCount <- length(DOIcolumn)
  
  # randomly add time delays between downloads with a mean random 
  # delay of 4 seconds and a range of 1 to 20 seconds
  if(buffer == TRUE) {
    timeLags <- rnbinom(totalCount, mu = 4, size = 10) + 1
  } else {
    timeLags <- rep(0.0, totalCount)  
  }

  message("--- Starting download attempts of ", totalCount, " DOI --- " )
  downloadCount <- 1
  downloadOutcomes <- mapply(
    function(DOI, directory, theFileName, lagTime, validatePDF, quiet) {
      Sys.sleep(lagTime)
      message(paste0("\nHarvesting ", downloadCount, " of ", totalCount, ":"))
	  if(quiet == FALSE) message("         ", appendLF = FALSE)
      downloadCount <<- downloadCount + 1
      PDF_download(DOI, directory, theFileName, validatePDF, quiet, WindowsProxy) 
    }, 
    DOIcolumn, 
    directory, 
    aDataFrame[, FileNamecolumn],
    timeLags,
    validatePDF,
    quiet
  )
  
  if(showSummary & !quiet) {
    message("\nPDF download summary:\n")
    a <- summary(as.factor(downloadOutcomes))
    for(i in 1:length(a)) message(paste("\t", a[i], "=", names(a[i])))
    message(paste0("\nDownloads located in: ", directory))
  }
  
  aDataFrame$downloadOutcomes <- downloadOutcomes
  return(aDataFrame)
}
