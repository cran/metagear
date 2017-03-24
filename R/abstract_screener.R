#' A GUI screener to quickly code candidate studies for inclusion/exclusion into
#' a systematic review or meta-analysis.
#'
#' A GUI screener to help scan and evaluate the title and abstract of studies to 
#' be included in a systematic review or meta-analysis.    
#'
#' @param file The file name and location of a .csv file containing the 
#'    abstracts and titles.  The .csv file should have been initialized with
#'    \code{effort_initialize} and populated with screeners (reviewers) using
#'    \code{effort_distribute}.
#' @param aReviewer The name (a string) of the reviewer to screen abstracts.  
#'    It is used when there are multiple reviewers assigned to screen abstracts.
#'    The default column label is "REVIEWERS" as initialized with 
#'    \code{effort_distribute}.
#' @param reviewerColumnName The name of the column heading in the .csv file 
#'    that contains the reviewer names that will screen abstracts.  The default 
#'    column label is "REVIEWERS". 
#' @param unscreenedColumnName The name of the column heading in the .csv file 
#'    that contains the screening outcomes (i.e. vetting outcomes by a reviewer). 
#'    Unscreened references are by default labeled as "not vetted".  The
#'    reviewer then can code to "YES" (is a relevant study), "NO" is not relevant
#'    and should be excluded, or "MAYBE" if the title/abstract is missing or
#'    does not contains enough information to fully assess inclusivity.
#'    The default label of this column is "INCLUDE".  
#' @param unscreenedValue Changes the default coding (a string) of "not vetted"
#'    that designates whether an abstract remains to be screened or vetted.   
#' @param abstractColumnName The name of the column heading in the .csv file 
#'    that contains the abstracts. The default label of this column is 
#'    "ABSTRACT".
#' @param titleColumnName The name of the column heading in the .csv file 
#'    that contains the titles. The default label of this column is "TITLE".
#' @param browserSearch Change the url for the browser title search; the 
#'    default is Google.
#' @param protect When \code{"TRUE"}, prevents the title and abstract from being
#'    clicked, selected or edited. 
#' @param fontSize Change the font size of the title and abstract text.
#' @param windowWidth Change the default width of the GUI window.
#' @param windowHeight Change the default height of the GUI window.
#' @param buttonSize Change the default size of the "YES" and "NO" buttons.
#'
#'
#' @return NULL
#'
#' @examples \dontrun{
#'
#' data(example_references_metagear)
#' effort_distribute(example_references_metagear, 
#'                   initialize = TRUE, reviewers = "marc", save_split = TRUE)
#' abstract_screener("effort_marc.csv", aReviewer = "marc")
#'}
#'
#' @note \strong{Installation and troubleshooting}\cr\cr Upon first use, 
#'    \code{abstract_screener} will download the gWidgets package
#'    and associated toolkits needed to build GUI interfaces.  A small window will
#'    also prompt you to download GTK+ asking "Need GTK+ ?".  From the listed
#'    options answer: "Install GTK+" and click 'OK'.  Once installed these will
#'    not be downloaded again.  Sometimes there is an issue with the installation
#'    of GTK+, see \url{http://www.learnanalytics.in/blog/?p=31} for advice based 
#'    on the \code{Rattle} R Package (both \code{Rattle} and \code{metagear} use
#'    the same GUI dependencies).  \cr\cr \strong{How to use the screener}
#'    \cr\cr The GUI itself will appear as a single window with the first 
#'    title/abstract listed in the .csv file. If abstracts have already been 
#'    screened/coded, it will begin at the nearest reference labeled as 
#'    "not vetted". The SEARCH WEB button opens the default browser and 
#'    searches Google with the title of the reference. The YES, MAYBE, NO 
#'    buttons, which also have shortcuts ALT-Y and ALT-N, are used to code the 
#'    inclusion/exclusion of the reference. Once clicked/coded the next 
#'    reference is loaded. The SAVE button is used to save the coding progress 
#'    of screening tasks. It will save coding progress directly to the 
#'    loaded .csv file. \strong{Closing the GUI and not saving will result in 
#'    the loss of screening efforts relative to last save.}  \cr\cr There is 
#'    also an ISSUE FIXES menu bar with quick corrections to screening errors. 
#'    These include ISSUE FIXES: REFRESH TITLE AND ABSTRACT TEXT which reloads 
#'    the text of the current abstract in case portions were deleted when 
#'    copying and pasting sections (this can be avoided if 
#'    \code{protect = TRUE} is enabled), ISSUE FIXES: STATUS OF CURRENT ABSTRACT 
#'    which provides information on whether or not the abstract was previously 
#'    screened, and ISSUE FIXES: RETURN TO PREVIOUS ABSTRACT that  
#'    backtracks to the previous abstract if a selection error occurred (note a 
#'    warning will appear of there is a change to its inclusion/exclusion 
#'    coding).        
#'
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @importFrom utils browseURL read.csv write.csv
#' @export abstract_screener

abstract_screener <- function(file = file.choose(),
                              aReviewer = NULL, 
                              reviewerColumnName = "REVIEWERS",
                              unscreenedColumnName = "INCLUDE",                             
                              unscreenedValue = "not vetted", 
                              abstractColumnName = "ABSTRACT",
                              titleColumnName = "TITLE",
                              browserSearch = "https://www.google.com/search?q=",
                              protect = FALSE,
                              fontSize = 13,
                              windowWidth = 700,
                              windowHeight = 510,
                              buttonSize = 50) {
  
  # get file with abstract
  aDataFrame <- read.csv(file, header = TRUE)
  
  # subset abstracts based on the current screener (aka 'reviewer')
  subData <- subset(aDataFrame, aDataFrame[reviewerColumnName] == aReviewer)
  subData <- data.frame(lapply(subData, as.character), stringsAsFactors = FALSE)
  
  # check if all abstracts have been already vetted
  if(unscreenedValue %in% subData[, unscreenedColumnName] ) {
    # start screener at first unvetted abstract
    currentItem <- max.col(t(subData[unscreenedColumnName] == unscreenedValue), 
                           "first")
  } else {
    .metagearPROBLEM("error",
                     paste("all abstracts have already been screened, 
                           no more abstracts coded as:", unscreenedValue))
  }
  
  
  options("guiToolkit" = "RGtk2")
  
  # warning confirmation box used when a reviewer revisits and edits a previously
  # screened reference
  confirmDialog <- function(theValue, oldValue, handler = NULL) {
    aWindow <- gwindow("Warning", width = 50, height = 50)
    aGroup <- ggroup(container = aWindow)
    gimage("info", dirname = "stock", size = "dialog", container = aGroup)
    inner.group <- ggroup(horizontal = FALSE, container = aGroup)
    theComment <- paste0("Previously screened as: ", 
                         subData[currentItem, unscreenedColumnName], 
                         "\rDo you wish to update to: ", theValue
    )
    glabel(theComment, container = inner.group, expand = TRUE)
    button.group <- ggroup(container = inner.group)
    addSpring(button.group)
    gbutton(paste("update to:", theValue), 
            handler = function(h, ...) {
              updateAll(theValue, ...)
              dispose(aWindow)
            }, container = button.group)
    gbutton(paste("keep as:", oldValue), 
            handler = function(h, ...) {
              updateAll(theValue = oldValue, ...)
              dispose(aWindow)
            }, container = button.group)
    return()
  }
  
  # a status box used when a reviewer wants to see if the abtracts was previously
  # screened
  statusDialog <- function(handler = NULL) {
    aWindow <- gwindow("Abstract Status", width = 50, height = 50)
    aGroup <- ggroup(container = aWindow)
    inner.group <- ggroup(horizontal = FALSE, container = aGroup)
    glabel(paste("SCREENING OUTCOME:", 
                 subData[currentItem, unscreenedColumnName]), 
           container = inner.group, expand = TRUE)
    button.group <- ggroup(container = inner.group)
    addSpring(button.group)
    gbutton("return", 
            handler = function(h, ...) {
              dispose(aWindow)
            }, container = button.group)
    return()
  }
  
  # used to update the reference list
  theAnswer <- function(theValue, ...) {
    if(subData[currentItem, unscreenedColumnName] != unscreenedValue) {
      confirmDialog(theValue, oldValue = subData[currentItem, unscreenedColumnName])
    } else {
      updateAll(theValue, ...)
    }
  }
  
  # helper function used to update and keep track of screened abstracts
  updateAll <- function(theValue, ...) {
    
    if(currentItem <= nrow(subData)) {
      subData[[currentItem, unscreenedColumnName]] <<- theValue
      currentItem <<- currentItem + 1
    }
    
    if(currentItem > nrow(subData)) {
      svalue(text_ABSTRACT) <- "You have screened all the Abstracts!"
      svalue(text_TITLE) <- ""
    } else {
      svalue(text_ABSTRACT) <- subData[currentItem, abstractColumnName] 
      svalue(text_TITLE) <- subData[currentItem, titleColumnName] 
      svalue(text_progress) <- paste0(
        round(((currentItem - 1.0)/nrow(subData)) * 100, digits = 1),
        "% complete (", currentItem, " of ", nrow(subData), ")")
    }
  }
  
  # helper function used return to previous abstract
  backtrack <- function(...) {
    
    if(currentItem != 1) {
      currentItem <<- currentItem - 1
      svalue(text_ABSTRACT) <- subData[currentItem, abstractColumnName] 
      svalue(text_TITLE) <- subData[currentItem, titleColumnName] 
      svalue(text_progress) <- paste0(
        round(((currentItem - 1.0)/nrow(subData)) * 100, digits = 1),
        "% complete (", currentItem, " of ", nrow(subData), ")")
    }
    
  }
  
  
  # START of SCREENER GUI
  
  win <- gwindow("metagear: Abstract Screener", visible = TRUE)
  
  paned <- ggroup(container = win, horizontal = FALSE)
  
  frame_TITLE <- gframe("Title", container = paned, horizontal = TRUE)
  
  text_TITLE <- gtext(subData[currentItem, titleColumnName], 
                      container = frame_TITLE, 
                      expand = TRUE, 
                      font.attr = list(style = "bold", size = fontSize))
  size(text_TITLE) <- c(windowWidth, 70)
  
  if(protect == TRUE) enabled(text_TITLE) <- FALSE
  
  addSpace(frame_TITLE, 2)
  
  aButton_webSearch <- gbutton("Search\n Web", 
                               container = frame_TITLE, 
                               handler = function(h, ...) 
                                 browseURL(paste0(browserSearch, 
                                                  subData[currentItem, titleColumnName]))) 
  size(aButton_webSearch) <- c(50, 40)
  
  addSpace(frame_TITLE, 5)
  
  # end of frame_TITLE
  
  frame_ABSTRACT <- gframe("Abstract", container = paned, horizontal = FALSE) 
  
  text_ABSTRACT <- gtext(subData[currentItem, abstractColumnName], 
                         container = frame_ABSTRACT, 
                         expand = TRUE, 
                         font.attr = list(style = "bold", size = fontSize))

  size(text_ABSTRACT) <- c(windowWidth + 50, windowHeight)
  
  if(protect == TRUE) enabled(text_ABSTRACT) <- FALSE
  
  
  # end of frame_ABSTRACT
  
  buttons_paned <- ggroup(container = paned)
  
  aButton_YES <- gbutton("YES", 
                         container = buttons_paned, 
                         handler = function(h, ...) 
                           theAnswer(theValue = "YES", ...))
  size(aButton_YES) <- c(buttonSize, buttonSize)
  
  aButton_MAYBE <- gbutton("MAYBE", 
                           container = buttons_paned, 
                           handler = function(h, ...) 
                             theAnswer(theValue = "MAYBE", ...))
  
  aButton_NO <- gbutton("NO", 
                        container = buttons_paned, 
                        handler = function(h, ...) 
                          theAnswer(theValue = "NO", ...))
  size(aButton_NO) <- c(buttonSize, buttonSize)
  
  addSpace(buttons_paned, 50)
  
  frame_PROGRESS <- gframe("Progress", container = buttons_paned, expand = TRUE)
  
  addSpace(frame_PROGRESS, 10)
  
  text_progress <- glabel(paste0("Reviewer: ", 
                                 aReviewer, 
                                 "  |  ", 
                                 round(((currentItem - 1.0)/nrow(subData)) * 100, digits = 1),
                                 "% complete (", currentItem, " of ", 
                                 nrow(subData) , ")"), 
                          container = frame_PROGRESS)
  
  addSpace(frame_PROGRESS, 10)
  
  aButton_save <- gbutton("SAVE", 
                          container = frame_PROGRESS, 
                          handler = function(h, ...) {
                            write.csv(subData, 
                                      file = file, #file was dataFilename
                                      row.names = FALSE)
                            svalue(text_lastSaved) <- paste("last saved: ", Sys.time())
                          }
  )
  size(aButton_save) <- c(60, 20)
  text_lastSaved <- glabel(paste("last saved: not this session"), 
                           container = frame_PROGRESS)
  
  #end of frame_PROGRESS
  
  #end of buttons_paned
  
  #end of paned
  
  
  # start menu bar
  
  menuBar <- list()
  menuBar$"Issue Fixes"$"Refresh Title and Abstract Text"$handler = 
    function(h,...) {
      svalue(text_ABSTRACT) <- subData[currentItem, abstractColumnName] 
      svalue(text_TITLE) <- subData[currentItem, titleColumnName] 
    }
  menuBar$"Issue Fixes"$"Status of Current Abstract"$handler = 
    function(h, ...) {
      statusDialog(...)
    }
  menuBar$"Issue Fixes"$"Return to Previous Abstract"$handler = 
    function(h, ...) {
      backtrack(...)
    }
  mb <- gmenu(menuBar, container = win)
  
  # end menu bar
  
  
  visible(win) <- TRUE
  
  # end of win
} 