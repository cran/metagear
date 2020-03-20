#' Attempts to download a PDF using a DOI link.
#'
#' Tries to download a PDF file using the digital objected identifier (DOI) link.
#' Uses ad hoc searches of journal HTML pages to detect candidate PDFs for
#' download.  Downloads all candidate pdfs. \strong{If running downloader
#' in Windows, having \code{"WindowsProxy = TRUE"} will significantly improve
#' download success.}
#'
#' @param DOI A string of the DOI (digital object identifier) used to identify
#'    the source of a journal article PDF file(s).
#' @param directory A string of the location (directory) were downloaded PDF
#'    files are to be saved.  Directory name must end with "\\\\".
#' @param theFileName Used to rename the downloaded file.  No need to include
#'    extension ".pdf".
#' @param validatePDF When \code{"TRUE"} will only save to files that are valid
#'    PDF documents.  When \code{"FALSE"} will save all candidate files, even if
#'    they are not valid PDF formats.
#' @param quiet When \code{"FALSE"} does not print to console download progress
#'    and summary.
#' @param WindowsProxy When \code{TRUE} significantly improves download success
#'    for computers running Windows; when \code{FALSE} on a Windows based
#'    computer, you may only be able to download 30 to 50 PDFs at a time before
#'    a connection error occurs and halts all downloads (e.g.,
#'    \code{InternetOpenUrl failed} error).
#'
#' @return A string describing the download success.  If unsuccessful,
#'    returns the type of error during the download attempt.
#'
#' @seealso \code{\link{PDFs_collect}}
#'
#' @importFrom stringr str_extract str_replace_all
#' @importFrom utils download.file
#' @export PDF_download

PDF_download <- function(DOI,
                         directory = getwd(),
                         theFileName = "temp",
                         validatePDF = TRUE,
                         quiet = FALSE,
                         WindowsProxy = FALSE) {

  if(!quiet) {
    message(paste0("Collecting PDF from DOI: ", DOI))
    message(paste0("\t\t\tExtraction 1 of 2: HTML script...."), appendLF = FALSE)
  }

  if(is.URLconnectable(paste0("http://dx.doi.org/", DOI))) {
    urlMessage <- " successful"
    theHTMLvector <- getHTMLfromURL(paste0("http://dx.doi.org/", DOI))

    if(!quiet) {
      message(paste0(urlMessage))
      message(paste0("\t\t\tExtraction 2 of 2: PDF download..."), appendLF = FALSE)
    }

    wasPDFdownloaded <- extractPDFsFromHTML(theHTMLvector,
                                            directory,
                                            theFileName,
                                            validatePDF,
                                            WindowsProxy)

    if(wasPDFdownloaded == TRUE) {
      downloadMessage <- " successful"
      downloadOutcome <- "downloaded"
    } else {
      downloadMessage <- wasPDFdownloaded
      downloadOutcome <- "download error"
    }

     if(!quiet) message(paste0(downloadMessage,
                               ifelse(downloadOutcome == " downloaded",
                                      paste0(" (filename: ", theFileName, ".pdf)"), "")))


  } else {
    urlMessage <- " cannot open: HTTP status was '404 Not Found'"
    downloadMessage <- " skipped"

    if(!quiet) {
      message(paste0(urlMessage))
      message(paste0("\t\t\tExtraction 2 of 2: PDF download...",
                     downloadMessage))
    }

    if(is.na(DOI)) {
      downloadOutcome <- "no DOI"
    } else {
      downloadOutcome <- "URL error"
    }
  }

  return(downloadOutcome)
}


