#' Evaluates whether a file is a PDF document.
#'
#' Checks if provided file is in Portable Document Format (PDF).    
#'
#' @param aFileName A string that identifies a file name (and directory path) of
#'   the PDF candidate.  
#' @param verbose Provides more elaborate description of why the file could not 
#'   be evaluated as a PDF (e.g., when validating a PDF online). When 
#'   \code{"quiet"}, an error message is not generated.   
#'
#' @return A logical value indicating whether the file is a PDF document.
#'
#' @importFrom stringr str_extract
#' @export isPDF

isPDF <- function(aFileName,
                  verbose = TRUE) {
				  
  fileContents <- suppressWarnings(try(
                    readLines(aFileName, n = 5, ok = TRUE, warn = FALSE), 
                    silent = TRUE))

  # error catch when downloading PDFs online						
  if(inherits(fileContents, "try-error")) {
    if(verbose == TRUE) {
      message(paste0("Failed validation, with error: ", fileContents[1]))
    } else {
      if(verbose ==  "quiet") return(FALSE)
      # when verbose = FALSE, secret message intended for PDF_downloader success
      message(" poor internet connection,", appendLF = FALSE)
    }
    return(FALSE)
  }
  
  return(any(!is.na(str_extract(fileContents, ".*%PDF-1.*"))))
}

