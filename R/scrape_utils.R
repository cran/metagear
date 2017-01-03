# constructor for WOS HTML wildcard searches
newScrape <- function(wildcard_PRE = "",
                      wildcard_POST = "",
                      scrubs = NA,
                      collapse = FALSE,
                      aNumber = FALSE,
                      split = NA, 
                      multiline = FALSE) {
  
  list("wildcard_PRE" = wildcard_PRE,
       "wildcard_POST" = wildcard_POST,
       "scrubs" = scrubs,
       "collapse" = collapse,
       "isNumeric" = aNumber,
       "split" = split,
       "multiline" = multiline) 
  
}

# a list of WOS bibliographic items and their wildcard search and scrub patterns
scrapeWOSList <- list(
  author          = newScrape("Find more records by this author\"", ".*\\((.+?)\\).*", collapse = TRUE, split = "alt"),
# author          = newScrape("field=AU&amp;value=", ".*field=AU&amp;value=(.+?)\".*"), # single author papers
# authors         = newScrape("author_name=", ".*author_name=(.+?)&amp;.*"), # multi-author papers
  year            = newScrape("publication_year", ".*publication_year%253D(.+?)%2526.*", aNumber = TRUE),
  title           = newScrape("title%253D", ".*title%253D(.+?)%2526.*", scrubs = "%252520"),
  journal         = newScrape("journal%253D", ".*journal%253D(.+?)%2526.*", scrubs = "%252520"),
# journal         = newScrape("LinksService&jTitle=", ".*LinksService&jTitle=(.+?)&link_type.*"),
  volume          = newScrape("%252Evolume%253D", ".*volume%253D(.+?)%2526.*"),
  pages           = newScrape("%252Epages%253D", ".*pages%253D(.+?)%2526.*"),
  DOI             = newScrape("dx.doi.org", ".*dx.doi.org/(.+?)&amp;.*"),
  abstract        = newScrape("FR_field", ".*FR_field\">(.+?)</p>.*", scrubs = "<br>", collapse = TRUE), # multiline = TRUE
  N_references    = newScrape("<a title=.*View this record.*s bibliography", ".*<b>(.+?)</b>.*", aNumber = TRUE),
  N_citations     = newScrape("<a title=.*View all of the articles that cite this one", ".*<b>(.+?)</b>.*", aNumber = TRUE),
  journal_IF      = newScrape("Impact_Factor_table", ".*<tr> <td>(.+?)</td> <td>.*", aNumber = TRUE),
  journal_IF_year = newScrape("Impact_Factor_table", ".*<tr> <th>(.+?)</th> <th>.*", aNumber = TRUE)
)
# scrapes WOS HTML to locate and scrub bibliographic data
scrapeHTML_WOS <- function(theHTMLdata,
                           wildcard_PRE, 
                           wildcard_POST,
                           scrubs = NA,
                           collapse = FALSE,
                           isNumeric = FALSE, 
                           split = NA,
                           multiline = FALSE) {
  
  # PRE-scrub of HTML
  preScrub <- unique(theHTMLdata[!is.na(str_extract(theHTMLdata, wildcard_PRE))])
  if(collapse == TRUE) preScrub <- paste(preScrub, collapse = "")
  if(!is.na(split)) preScrub <- unlist(str_split(preScrub, split))

  # abstract only: working progress: recover multiline abstracts with HTML line 
  # breaks, must remove collapse = TRUE for this to work.  Only tested with 
  # Journal of Ecology. Does not work with Diversity and Distributions. 
  # Note: 12/20/2016
  if(multiline == TRUE) {

    firstSentence <- word(preScrub, -1)
    firstSentence <- firstSentence[!is.na(str_extract(firstSentence, "\\."))]
    newpreScrub <- theHTMLdata[!is.na(str_extract(theHTMLdata, firstSentence))]
    theStart <- which(!is.na(str_extract(theHTMLdata, firstSentence)))

    aSentence <- 0; finished <- FALSE;
    while(finished == FALSE) {
      if (aSentence == 0) {
        theAbstract <- theHTMLdata[theStart]
      } else {
        isLast <- str_extract(theHTMLdata[theStart + aSentence], ".</p>")
        theAbstract <- paste(theAbstract, theHTMLdata[theStart + aSentence])
        if(!is.na(isLast)) finished <- TRUE
      }
      
      aSentence <- aSentence + 1
    }
    
    preScrub <- theAbstract
  }
  
  # POST-scrub of HTML
  theText <- unique(gsub(wildcard_POST, "\\1", preScrub))
  
  # additional scrubbing
  if(!is.na(scrubs)) theText <- gsub(scrubs, " ", theText)
  
  if(isNumeric == TRUE) {
    theText <- as.numeric(theText)
    if(length(theText) == 0) theText <- 0
  }
  
  return(theText)
}
