#' Attempts to scrape/extract bibliographic data from Web of Science.
#'
#' A not so elegant way to extract bibliographic data of a research article by
#' scraping the contents of Web of Science (WOS).  Requires the DOI (digital
#' object identifier) of an article, as well as web access with an institutional
#' subscription to WOS.  Note: This function is not suited to extract data
#' for book chapters available on WOS.  Current extractions include: a vector
#' of authors (author), publication year (year), article title (title), journal
#' title (journal), journal volume (volume), page numbers (pages), abstract
#' (abstract), number of references (N_references), number of citations
#' (N_citations), journal impact factor (journal_IF), and the year the journal
#' impact factor was released (journal_IF_year).  Finally the date of the scrape
#' is also provided (date_scraped). Bulleted abstracts or those with subheadings
#' or subparagraphs will not be extracted properly.
#'
#' @param DOI A string as the DOI (digital object identifier) of a research article.
#' @param quiet When \code{TRUE}, does not print an MLA-style reference of the
#'    extracted article.
#'
#' @return A list of bibliographic extractions and a timestamp of the scrape.
#'
#' @examples \dontrun{
#'
#' # use DOI to scrape number of WOS citations of a research article
#' data(example_references_metagear)
#' someRefs <- effort_initialize(example_references_metagear)
#' theWOSRef <- scrape_bibliography(someRefs$DOI[1])
#' print(paste("citations = ", theWOSRef$N_citations))
#'
#' }
#'
#' @importFrom stringr str_extract str_split word
#' @export scrape_bibliography

scrape_bibliography <- function(DOI,
                                quiet = FALSE) {

  WOSurl <- "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/"
  data <- readLines(paste0(WOSurl, DOI), n = -1, ok = TRUE, warn = FALSE)

  # check if WOS has record of DOI
  if(length(unique(data[!is.na(str_extract(data, "No matching items found"))])) > 0) {
    message("WOS does not have a record of the DOI.")
    return(NULL)
  }

  theBiblio <- lapply(scrapeWOSList,
                      function(x, theData) scrapeHTML_WOS(theData,
                                                          x$wildcard_PRE,
                                                          x$wildcard_POST,
                                                          x$scrubs,
                                                          x$collapse,
                                                          x$isNumeric,
                                                          x$split,
                                                          x$multiline,
                                                          x$skipline),
                      theData = data)

  theBiblio <- c(theBiblio, list("date_scraped" = Sys.Date()))

  # TO DO: squeeze into separate scrub function
  # clean authorship
  theBiblio$author <- theBiblio$author[-1]
  # clean title
  theBiblio$title <- gsub("%25253F", "?", theBiblio$title) # switch to ?
  theBiblio$title <- gsub("%25253A", ":", theBiblio$title) # switch to ?
  theBiblio$title <- gsub("%25253",  ": ", theBiblio$title) # switch to :
  theBiblio$title <- gsub("%25252C",  ",", theBiblio$title) # switch to ,
  # clean journal
  theBiblio$journal <- gsub("%252526", "&", theBiblio$journal) # switch to &

  if(!quiet) {

    if(length(theBiblio$author) > 1) {
      theAuthors <- str_split(theBiblio$author, ",")[[1]][1]
      theAuthors <- paste0(theAuthors, " et al.")
    } else {
      theAuthors <- theBiblio$author
    }

    theCitation <- paste0(theAuthors,
                          " (", theBiblio$year, ") ",
                          theBiblio$title, ". ",
                          theBiblio$journal, " ",
                          theBiblio$volume, ", ",
                          theBiblio$pages, ".  ")
  }

  theBiblio$citation <- theCitation

  return(theBiblio)
}
