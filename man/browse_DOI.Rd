% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/browse_DOI.R
\name{browse_DOI}
\alias{browse_DOI}
\title{Opens a web page associated with a DOI (digital object identifier).}
\usage{
browse_DOI(theDOI, host = "DOI")
}
\arguments{
\item{theDOI}{A string that identifies an electronic document on the web.}

\item{host}{A string that defines the domain link used to open the DOI.  The 
default, \code{"DOI"}, will open to the web page associated with 
the DOI (e.g., publisher website).  Other options include \code{"WOS"} that 
will open the DOI in Web of Science, \code{"GS"} in Google Scholar, and 
\code{"CRF"} in Crossref.}
}
\description{
Uses the DOI name of a study reference to locate the e-journal website,
   or reference/citation website in Web of Science, Google Scholar, or 
   CrossRef.  Opens in default web-browser.
}
\examples{
\dontrun{

browse_DOI("10.1086/603628")        
}

}
