#################################
### PDF html extraction functions
#################################

# evaluate whether the URL is available
is.URLconnectable <- function(theURL) {
  aConnection <- suppressWarnings(try(url(theURL,
                                          open = "rb"),
                                      silent = TRUE))
  if(inherits(aConnection, "try-error")) return(FALSE)
  close(aConnection)
  return(TRUE)
}



# collect HTML as large vector
getHTMLfromURL <- function(theURL) {
  theHTMLvector <- tryCatch(
    readLines(theURL, n = -1, ok = TRUE, warn = FALSE),
    error = function(cond) return(paste0(" failed url, with error: ", cond[1])),
    warning = function(cond) return(paste0(" maybe failed url, with warning: ", cond[1]))
  )

  # scrub unlikely url-containing rows with short HTML strings
  theHTMLvector <- theHTMLvector[nchar(theHTMLvector) > 25]
  return(theHTMLvector)
}

# generic constructor for url searches of PDFs in HTML documents
aPattern <- function(wildcard_PRE = "",
                     wildcard_POST = "",
                     wildcard_POST_add = "",
                     wildcard_POST_remove = "",
                     domain = "",
                     anchor = "",
                     multiURLs = "",
                     redirect = "",
                     HTMLredirect = "") {

  list("wildcard_PRE" = wildcard_PRE,
       "wildcard_POST" = wildcard_POST,
       "wildcard_POST_add" = wildcard_POST_add,
       "wildcard_POST_remove" = wildcard_POST_remove,
       "domain" = domain,
       "anchor" = anchor,
       "multiURLs" = multiURLs,
       "redirect" = redirect,
       "HTMLredirect" = HTMLredirect
  )

}

# a list of publishers and their ad hoc wildcard HTML search and scrub patterns
publisherList <- list(

  # Springer, Sage, ...
  wcf_generic         = aPattern(wildcard_PRE = "http.*pdf",
                                 multiURLs = " "),

  # Elsevier,
  wcf_elsevier        = aPattern(wildcard_PRE = "content=\"http.*pdf",
                                 wildcard_POST = "content=\"",
                                 multiURLs = " ",
                                 HTMLredirect = " ",
                                 redirect = "https://pdf.sciencedirectassets.com.*=client"),

  # Elsevier,
  wcf_elsevier2        = aPattern(wildcard_PRE = "<meta name=\"citation_pii\" content=\".*\"",
                                  wildcard_POST = "<meta name=\"citation_pii\" content=\"",
                                  wildcard_POST_remove = ".{1}$",
                                  HTMLredirect = " ",
                                  #redirect = "https://pdf.sciencedirectassets.com.*=client",
                                  domain = "https://www.sciencedirect.com/science/article/pii/",
                                  anchor = "/pdfft?isDTMRedir=true&amp;download=true"),

  # JSTOR
  wcf_jstore          = aPattern(wildcard_PRE = "pdf/.*pdf\" target",
                                 wildcard_POST = "\" target",
                                 domain = "http://www.jstor.org/stable/",
                                 anchor = "?acceptTC=true"),

  # Taylor & Francis Online
  wcf_tfo             = aPattern(wildcard_PRE = "/doi/pdf/.*?needAccess=true",
                                 domain = "http://www.tandfonline.com"),

  # American Chemical Society
  # NOTE: ACS provides many PDF links on their websites (all of these will be
  # downloaded).
  wcf_acs             = aPattern(wildcard_PRE = "/doi/pdf/.*\">PDF<",
                                 wildcard_POST = "\">PDF<",
                                 domain = "http://pubs.acs.org"),

  # NRC Research Press
  wcf_nrc             = aPattern(wildcard_PRE = "/doi/pdf/.*\">",
                                 wildcard_POST = "\">",
                                 domain = "http://www.nrcresearchpress.com"),

  # BioOne
  wcf_bioone          = aPattern(wildcard_PRE = "/doi/pdf/.*\">PDF",
                                 wildcard_POST = "\">PDF",
                                 domain = "http://www.bioone.org"),

  # University of Chicago Press
  wcf_chicago         = aPattern(wildcard_PRE = "/doi/pdf/.*\" class=\"ctrl",
                                 wildcard_POST = "\" class=\"ctrl",
                                 domain = "http://www.journals.uchicago.edu"),

  # Nature
  wcf_nature          = aPattern(wildcard_PRE = "=.*\\.pdf",
                                 wildcard_POST = "=\"",
                                 domain = "http://www.nature.com"),
  wcf_nature2         = aPattern(wildcard_PRE = "=.*\\.pdf",
                                 wildcard_POST = "=\"download-pdf\"><a href=\"",
                                 domain = "http://www.nature.com"),
  wcf_nature3         = aPattern(wildcard_PRE = "/articles/.*.pdf",
                                 multiURLs = " ",
                                 domain = "http://www.nature.com"),

  # Maney Online (now Taylor & Francis)
  wcf_maney           = aPattern(wildcard_PRE = "/.*pdfplus.*\"",
                                 wildcard_POST = "\"",
                                 domain = "http://www.maneyonline.com"),

  # Japan Science and Technology Aggregator
  wcf_jstage          = aPattern(wildcard_PRE = "/article.*/_pdf",
                                 domain = "https://www.jstage.jst.go.jp"),

  # PLOS
  wcf_plos            = aPattern(wildcard_PRE = "http.*type=printable"),

  # Wiley Online
  wcf_ScienceDirect   = aPattern(wildcard_PRE = "http://onlinelibrary.wiley.com/doi/.*/abstract",
                                 wildcard_POST = "abstract",
                                 wildcard_POST_add = "pdf",
                                 multiURLs = "\"",
                                 redirect = "http://onlinelibrary.wiley.com/store/.*pdf.*"),
  # BioMed Central
  wcf_BioMed          = aPattern(wildcard_PRE = "http:.*?site=",
                                 wildcard_POST = "\\?site="),

  # Chemical Society of Japan Journals
  wcf_CSJ             = aPattern(wildcard_PRE = "/doi.*\" class",
                                 wildcard_POST = "\" class",
                                 domain = "http://www.journal.csj.jp")

)

# scrapes HTML document to find PDFs using url patterns
scrapeHTML <- function(theHTMLdata,
                       wildcard_PRE,
                       wildcard_POST,
                       wildcard_POST_add,
                       wildcard_POST_remove,
                       domain,
                       anchor,
                       multiURLs,
                       redirect,
                       HTMLredirect) {

#message(theHTMLdata[1:2])
  if(HTMLredirect != "") {
    someHTML <- unique(theHTMLdata[which(!is.na(str_extract(theHTMLdata, "\"redirectURL")))])
    getHTML_Link <- gsub(".*%3A%2F%2F(.+?)%3Fvia.*", "\\1", someHTML)
    getHTML_Link <- paste0("https://", stringr::str_replace_all(getHTML_Link, "%2F", "/"))
    getHTML_Link <- stringr::str_replace_all(getHTML_Link, "&amp;", "&")
    theHTMLdata <- getHTMLfromURL(getHTML_Link)
    #someHTML <- unique(theHTMLdata[which(!is.na(str_extract(theHTMLdata, "https://pdf.")))])
    #getHTML_Link <- gsub(".*window.location = '(.+?)'%3Fvia.*';", "\\1", someHTML)
  }


  # PRE-scrub of HTML
  candidateURLs <- pullLinks(theHTMLdata, wildcard_PRE)

  # if nothing found return NULL
  if(length(candidateURLs) == 0) return(NULL)

  # find all URLs within each html line
  if(multiURLs != "") {
    tempURLs <- vector()
    for(i in candidateURLs) {
      foundURLs <- pullLinks(unlist(strsplit(i, multiURLs)), wildcard_PRE)
      if(!identical(foundURLs, character(0))) tempURLs <- c(tempURLs, foundURLs)
    }
    candidateURLs <- tempURLs
  }

  # POST-scrub of urls
  if(wildcard_POST != "") candidateURLs <- gsub(wildcard_POST,
                                                wildcard_POST_add,
                                                candidateURLs)

  # POST-scrub of bottom urls
  if(wildcard_POST_remove != "") candidateURLs <- gsub(wildcard_POST_remove,
                                                       "",
                                                       candidateURLs)

  # add domain to candidate urls
  if(domain != "") candidateURLs <- paste0(domain, candidateURLs)

  # add anchor to candidate urls
  if(anchor != "") candidateURLs <- paste0(candidateURLs, anchor)

  # remove any duplicates or invalid urls containing spaces
  candidateURLs <- unique(candidateURLs[!grepl(" ", candidateURLs)])
  if(length(candidateURLs) == 0) return(NULL)

  if(redirect != "") {
    redirectedHTMLdata <- getHTMLfromURL(candidateURLs)
    candidateURLs <- unique(scrapeHTML(theHTMLdata = redirectedHTMLdata,
                                       wildcard_PRE = redirect,
                                       wildcard_POST = "",
                                       wildcard_POST_add = "",
                                       wildcard_POST_remove = "",
                                       domain = "",
                                       anchor = "",
                                       multiURLs = multiURLs,
                                       redirect = "",
                                       HTMLredirect = ""))
  }


  return(candidateURLs)
}

# common HTML extractor
pullLinks <- function(theHTMLdata, wildcard) {

  someLinks <- str_extract(theHTMLdata, wildcard)
  return(unique(someLinks[!is.na(someLinks)]))

}

# iterate through candidate URLs and test validity
extractPDFsFromHTML <- function(theHTMLvector,
                                theDirectory,
                                theFileName,
                                validatePDF,
                                WindowsProxy
                                ) {

  candidateLinks <- extractPDFLinksFromHTML(theHTMLvector, validatePDF)

  switch(candidateLinks[1],
    "error_url" = return (" failed, url connections too slow or unavailable"),
    "error_pdf" = return (" failed, connections too slow or files not PDF format"),
    "error_links" = return (" failed, no valid url links detected")
  )

  if(length(candidateLinks) != 1) theFileName <- paste0(theFileName,
                                                        ".",
                                                        1:length(candidateLinks))

  downloadOutcomes <- mapply(
    getPDFfromURL,
    candidateLinks,
    theDirectory,
    theFileName,
    WindowsProxy
  )

  #return TRUE if at least one file was successfully downloaded
  return(any(downloadOutcomes == 0))
}

extractPDFLinksFromHTML <- function(theHTMLvector,
                                    validatePDF = TRUE) {

  candidateLinks <- lapply(publisherList,
                           function(x, theData) scrapeHTML(theData,
                                                           x$wildcard_PRE,
                                                           x$wildcard_POST,
                                                           x$wildcard_POST_add,
                                                           x$wildcard_POST_remove,
                                                           x$domain,
                                                           x$anchor,
                                                           x$multiURLs,
                                                           x$redirect,
                                                           x$HTMLredirect),
                           theData = theHTMLvector)

  # extract all unique PDF-links using a collection of search criteria defined in wildcardFunctionList
  candidateLinks <- unique(unlist(candidateLinks))
  if(length(candidateLinks) == 0) return ("error_links")

  # from the candidateLinks, select only those with connectable URLs
  candidateLinks <- candidateLinks[unlist(lapply(candidateLinks, is.URLconnectable))]
  if(length(candidateLinks) == 0) return ("error_url")

  # from the candidateLinks, select only those identified as PDFs
  if(validatePDF == TRUE) {
    candidateLinks <- candidateLinks[unlist(mapply(isPDF, candidateLinks, FALSE))]
    if(length(candidateLinks) == 0) return ("error_pdf")
  }

  return(candidateLinks)
}

# download PDF online
# NOTE: method = libcurl, helps with simultaneous downloads, but is more sensitive
# to errors with long urls with many anchors
getPDFfromURL <- function(theURL,
                          theDirectory,
                          theFileName,
                          WindowsProxy) {

  if(WindowsProxy == TRUE) {
    theProxy <- "wininet"
  } else {
    theProxy <- "auto"
  }

  aConnection <- suppressWarnings(try(download.file(theURL,
                                                    destfile = file.path(theDirectory,
                                                                         paste0(theFileName, ".pdf")),
                                                    quiet = TRUE,
                                                    method = theProxy,
                                                    mode = "wb",
                                                    cacheOK = FALSE,
                                                    options(timeout = 600)),
									  silent = TRUE))

  if(inherits(aConnection, "try-error"))
    return(paste0(" failed download, with error: ", aConnection[1]))

  return(aConnection)
}


#########################################
### within PDF Image extraction functions
#########################################

PDFobjectToImageFile <- function (objectLocation,
                                  theObjects,
                                  theFile,
                                  imageFileName) {

  # parse object by stream & endstream
  parsedImageObject <-  unlist(strsplit(theObjects[objectLocation], "stream"))

  # extract key char locations of image in PDF with trailingChars as a correction
  # for "stream" being followed by 2 return characters
  trailingChars <- "  "
  startImageLocation <- nchar(paste(parsedImageObject[1], "stream", trailingChars, sep = ""))
  endImageLocation <- startImageLocation + nchar(substr(parsedImageObject[2], 1, nchar(parsedImageObject[2]) - nchar("end")))
  PDFLocation <- nchar(paste(theObjects[1:(objectLocation - 1)], collapse = ''))

  # extract binary of image from PDF
  PDFImageBlock <- hexView::readRaw(theFile, offset = PDFLocation + startImageLocation, nbytes = endImageLocation, machine = "binary")

  # save binary of image to new file
  detectedImageFile <- file(imageFileName, "wb")
      writeBin(PDFImageBlock$fileRaw, detectedImageFile)
  close(detectedImageFile)

  # TO DO: RETURN INFO ABOUT SUCCESSFUL FILE SAVE
  return(imageFileName)
}
