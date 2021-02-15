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
#' @param fontSize Change the font gWidgets::size of the title and abstract text.
#' @param windowWidth Change the default width of the GUI window.
#' @param windowHeight Change the default height of the GUI window.
#' @param theButtons A vector of coding buttons included on the screener. The
#'    default is YES, maybe, and NO. Buttons can be removed as added by changing
#'    this vector. For example, theButtons = c("YES", "NO") to remove the
#'    maybe-button, or theButtons = c("YES", "maybe", NO", "model") to add a
#'    "model" button that tags studies specifically as "model".
#' @param keyBindingToButtons A vector of specific keyboard bindings to buttons.
#'    They are keyboard shortcuts to buttons and the default binding is y for
#'    YES-button, m for maybe-button, and n for NO-button. If theButtons parameter
#'    is modified then these keybindings should also be modified.
#' @param buttonSize Change the default gWidgets::size of buttons.
#' @param highlightKeywords A string or list of keywords that will be highlighted
#'    in title and abstract.
#' @param highlightColor The color of keywords highlighted in title and abstract.
#'    The default is blue, but for classic yellow use "palegoldenrod".
#'
#'
#' @return NULL
#'
#' @examples \dontrun{
#'
#' data(example_references_metagear)
#' effort_distribute(example_references_metagear,
#'                   initialize = TRUE,
#'                   reviewers = "marc",
#'                   save_split = TRUE)
#' abstract_screener("effort_marc.csv",
#'                   aReviewer = "marc",
#'                   highlightKeywords = "and")
#'}
#'
#' @note \strong{Installation and troubleshooting}\cr\cr For Mac OS users,
#'    installation is sometimes not straighforward as this screener requires the
#'    Tcl/Tk GUI toolkit to be installed. You can get this toolkit by making sure
#'    the latest X11 application (xQuartz) is installed, see here: 
#'    \url{https://www.xquartz.org/}. More information on
#'    installation is found in \code{metagear}'s vignette.
#'    \cr\cr \strong{How to use the screener} \cr\cr The GUI itself will appear
#'    as a single window with the first title/abstract listed in the .csv file.
#'    If abstracts have already been screened/coded, it will begin at the
#'    nearest reference labeled as "not vetted". The SEARCH WEB button opens the
#'    default browser and searches Google with the title of the reference. The
#'    YES, MAYBE, NO buttons, which also have keyboard shortcuts y and n, are
#'    used to code the inclusion/exclusion of the reference. Once clicked/coded
#'    the next reference is loaded. The SAVE button is used to save the coding
#'    progress of screening tasks. It will save coding progress directly to the
#'    loaded .csv file. \strong{Closing the GUI, and not saving, will result in
#'    the loss of screening efforts relative to last save.}  \cr\cr There is
#'    also an ISSUE FIXES menu bar with quick corrections to screening errors.
#'    These include ISSUE FIXES: REFRESH TITLE AND ABSTRACT TEXT which reloads
#'    the text of the current abstract in case portions were deleted when
#'    copying and pasting sections, ISSUE FIXES: STATUS OF CURRENT ABSTRACT
#'    which provides information on whether or not the abstract was previously
#'    screened, and ISSUE FIXES: RETURN TO PREVIOUS ABSTRACT that
#'    backtracks to the previous abstract if a selection error occurred (note a
#'    warning will appear of there is a change to its inclusion/exclusion
#'    coding).
#'
#' @import tcltk
#' @importFrom utils browseURL read.csv write.csv
#' @importFrom stringr str_locate_all
#' @export abstract_screener

abstract_screener <- function(file = file.choose(),
                              aReviewer = NULL,
                              reviewerColumnName = "REVIEWERS",
                              unscreenedColumnName = "INCLUDE",
                              unscreenedValue = "not vetted",
                              abstractColumnName = "ABSTRACT",
                              titleColumnName = "TITLE",
                              browserSearch = "https://www.google.com/search?q=",
                              fontSize = 13,
                              windowWidth = 70,
                              windowHeight = 16,
                              theButtons = c("YES", "maybe", "NO"),
                              keyBindingToButtons = c("y", "m", "n"),
                              buttonSize = 10,
                              highlightColor = "powderblue",
                              highlightKeywords = NA) {

  # get file with abstract
  aDataFrame <- read.csv(file, header = TRUE)

  # subset abstracts based on the current screener (aka 'aReviewer')
  subData <- subset(aDataFrame, aDataFrame[reviewerColumnName] == aReviewer)
  subData <- data.frame(lapply(subData, as.character), stringsAsFactors = FALSE)

  # check if all abstracts have been already screened
  if(unscreenedValue %in% subData[, unscreenedColumnName] ) {
    # start screener at first unvetted abstract
    currentItem <- max.col(t(subData[unscreenedColumnName] == unscreenedValue),
                           "first")
  } else {
    .metagearPROBLEM("error",
                     paste("all abstracts have already been screened,
                           no more abstracts coded as:", unscreenedValue))
  }

  # checks if tcltk is available and can be loaded
  if(requireNamespace("tcltk", quietly = TRUE)) {

    # default search string for keywords
    gsubTEXT <- paste0("(.{1,", windowWidth + 10, "})(\\s|$)")

    insert_tktext <- function(theTextWidget,
                              someText,
                              textFormat,
                              highlightKeywords,
                              highlightColor,
                              refresh = FALSE) {

      # add line breaks
      newText <- paste0("  ", gsub(textFormat, '\\1\n  ', someText))

      if(refresh == TRUE) tkdelete(theTextWidget, "1.0", "end")
      else tkinsert(theTextWidget, "1.0",  newText)

      if(anyNA(highlightKeywords)) {
        tkfocus(screenerWindow)
        return()
      }

      # add highlight tags
      theIndex <- 0
      thePos <- stringr::str_locate_all(pattern = paste(highlightKeywords, collapse = "|"),
                                        unlist(strsplit(newText, "\n")))
      for(i in thePos) {
        theIndex <- theIndex + 1
        if(length(i) != 0) {
          for(j in 1:(length(i)/2)) {
            if(refresh == TRUE) {
              tktag.delete(theTextWidget, paste0("aTag", theIndex))
            } else {
              tktag.add(theTextWidget,
                        paste0("aTag", theIndex),
                        paste0(theIndex, ".", i[j, 1] - 1),
                        paste0(theIndex, ".", i[j, 2]))
              tktag.configure(theTextWidget,
                              paste0("aTag", theIndex),
                              background = highlightColor)
            }
          }
        }
      }

      tkfocus(screenerWindow)
      return()
    }

    refresh_text <- function() {

      insert_tktext(titleText, subData[currentItem, titleColumnName],
                    gsubTEXT, highlightKeywords, highlightColor, refresh = TRUE)
      insert_tktext(titleText, subData[currentItem, titleColumnName], gsubTEXT,
                    highlightKeywords, highlightColor)
      insert_tktext(abstractText, subData[currentItem, abstractColumnName],
                    gsubTEXT, highlightKeywords, highlightColor, refresh = TRUE)
      insert_tktext(abstractText, subData[currentItem, abstractColumnName],
                    gsubTEXT, highlightKeywords, highlightColor)

      return()
    }

    confirmDialog <- function(theValue, oldValue) {

      updateTheValue <- tkmessageBox(type = "yesno",
                                     icon = "warning",
                                     title = "Warning",
                                     message = paste0("Previously screened as: ", oldValue,
                                                      "\rDo you wish to update to: ", theValue))

      if(as.character(updateTheValue) == "yes") updateAll(theValue)
      tkfocus(screenerWindow)

      return()
    }

    # a status box used when a reviewer wants to see if the abtracts was
    # previously screened
    statusDialog <- function() {

      tkmessageBox(title = "Current Abstract Status",
                   icon = "info",
                   message = paste0("SCREENING OUTCOME: ",
                                    subData[currentItem, unscreenedColumnName],
                                    "."))
      tkfocus(screenerWindow)

      return()
    }

    refreshDialog <- function() {

      refresh_text()

      return()
    }

    for(i in theButtons) {
      eval(parse(text = paste0(
      "theAnswer", i, " <- function() {\n",
        "if(subData[currentItem, unscreenedColumnName] != unscreenedValue) {\n",
          "confirmDialog(\"", i, "\", oldValue = subData[currentItem, unscreenedColumnName])\n",
        "} else {\n",
          "updateAll(\"", i, "\")\n",
        "}\n",
        "tkfocus(screenerWindow)\n",
      "}")))
    }

    # helper function used to update and keep track of screened abstracts
    updateAll <- function(theValue) {

      if(currentItem <= nrow(subData)) {
        subData[[currentItem, unscreenedColumnName]] <<- theValue
        currentItem <<- currentItem + 1
      }

      if(currentItem > nrow(subData)) {
        insert_tktext(abstractText, subData[currentItem, abstractColumnName],
                      gsubTEXT, highlightKeywords, highlightColor, refresh = TRUE)
        insert_tktext(abstractText, "You have screened all the Abstracts!",
                      gsubTEXT, highlightKeywords,  highlightColor)
        insert_tktext(titleText, subData[currentItem, titleColumnName],
                      gsubTEXT, highlightKeywords, highlightColor, refresh = TRUE)
      } else {
        refresh_text()
        tclvalue(theProgress) <- paste0("Reviewer: ",
                                        aReviewer,
                                        "\n",
                                        round(((currentItem - 1.0)/nrow(subData)) * 100, digits = 1),
                                        "% complete (", currentItem, " of ",
                                        nrow(subData) , ")")
      }
    }


    # helper function used return to previous abstract
    backtrack <- function() {

      if(currentItem != 1) {
        currentItem <<- currentItem - 1
        refresh_text()
        tclvalue(theProgress) <- paste0("Reviewer: ",
                                        aReviewer,
                                        "\n",
                                        round(((currentItem - 1.0)/nrow(subData)) * 100, digits = 1),
                                        "% complete (", currentItem, " of ",
                                        nrow(subData) , ")")
      }

    }

    searchBrowser <- function() browseURL(paste0(browserSearch,
                                                 subData[currentItem, titleColumnName]))

    saveProgress <- function() {
      write.csv(subData,
                file = file, #file was dataFilename
                row.names = FALSE)
      tclvalue(theSaveState) <- paste("last saved: ", Sys.time())
    }

    # START of SCREENER GUI

    screenerWindow <- tktoplevel()
    tktitle(screenerWindow) <- "metagear: Abstract Screener"

    # GUI text
    theTitle <- subData[currentItem, titleColumnName]
    theAbstract <- as.character(subData[currentItem, abstractColumnName])
    theProgress <- tclVar(paste0("Reviewer: ",
                                 aReviewer,
                                 "\n",
                                 round(((currentItem - 1.0)/nrow(subData)) * 100, digits = 1),
                                 "% complete (", currentItem, " of ",
                                 nrow(subData) , ")"))
    theSaveState <- tclVar(paste0("last saved: never"))

    ### MENU BAR

    screenerMenu <- tkmenu(screenerWindow)
    tkconfigure(screenerWindow, menu = screenerMenu)
    file_menu <- tkmenu(screenerMenu)
    tkadd(screenerMenu, "cascade", label = "Issue Fixes", menu = file_menu)
    tkadd(file_menu, "command",
          label = "Refresh Title and Abstract Text", command = refreshDialog)
    tkadd(file_menu, "command",
          label = "Status of Current Abstract", command = statusDialog)
    tkadd(file_menu, "command",
          label = "Return to Previous Abstract", command = backtrack)

    ### MAIN FRAME

    titleFrame <- ttklabelframe(screenerWindow, text = "Title", padding = 5)
    titleText <- tktext(titleFrame,
                        font = tkfont.create(size = fontSize),
                        height = 3, width = windowWidth)
    insert_tktext(titleText, theTitle, gsubTEXT, highlightKeywords, highlightColor)
    searchButton <- ttkbutton(titleFrame,
                              text = "\nSearch\n  Web\n",
                              command = searchBrowser, width = 10)
    tkgrid(titleText, searchButton, padx = 5)

    ### ABSTRACT FRAME SECTION

    abstractFrame <- ttklabelframe(screenerWindow, text = "Abstract", padding = 10)
    abstractScroll <- tkscrollbar(screenerWindow, orient = "vertical",
                                  command = function(...) tkyview(abstractText, ...))
    abstractText <- tktext(abstractFrame,
                           font = tkfont.create(size = fontSize),
                           height = windowHeight, width = windowWidth + 7,
                           yscrollcommand = function(...) tkset(abstractScroll, ...))
    insert_tktext(abstractText, theAbstract, gsubTEXT, highlightKeywords, highlightColor)
    tkgrid(abstractText, abstractScroll, sticky = "nsew")

    ### SELECTION & PROGRESS SECTION

    selectionFrame <- ttklabelframe(screenerWindow, text = "Is relevant?", padding = 10)


    for(i in theButtons)
      eval(parse(text = paste0(i, "Button <- ttkbutton(selectionFrame, text = \"\n",
                               i, "\n\"", ", command = theAnswer",
                               i, ", width = buttonSize)")))

    for(i in 1:length(theButtons))
      eval(parse(text = paste0("tkbind(screenerWindow, \"<",
                               keyBindingToButtons[i], ">\", theAnswer", theButtons[i], ")")))

    eval(parse(text = paste0("tkgrid(",
                             paste0(theButtons, "Button", collapse = ", "),
                             ", padx = 5)")))

    eval(parse(text = paste0("selectionLabel <- ttklabel(selectionFrame, text = \"\nNote: You can also press ",
                             paste0("'", head(keyBindingToButtons, -1), "'", collapse = ", "),
                             ", or ", "'", tail(keyBindingToButtons, 1),
                             "' on keyboard.\", font = tkfont.create(size = 7))\n")))

    eval(parse(text = paste0("tkgrid(selectionLabel, columnspan = 3)")))

    progressFrame <- ttklabelframe(screenerWindow, text = "Progress", padding = 10)
    progressLabel <- ttklabel(progressFrame, textvariable = theProgress)
    saveProgressButton <- ttkbutton(progressFrame,
                                    text = "Save",
                                    command = saveProgress,  width = 10)
    lastSaveLabel <- ttklabel(progressFrame, textvariable = theSaveState)
    tkgrid(progressLabel, saveProgressButton, lastSaveLabel, padx = 5)

    # build window content
    tkpack(titleFrame, abstractFrame, side = "top", pady = 10, padx = 10)
    tkpack(selectionFrame, side = "left", pady = 10, padx = 10)
    tkpack(progressFrame, fill = "x", side = "right", pady = 5, padx = 5)



    tkfocus(screenerWindow)

    # END of SCREENER GUI
  } else {

    .metagearPROBLEM("error",
                     paste("\n tcltk package is missing and is needed to generate the abstract screener GUI.",
                           "  --> If using Windows/Linux, try 'install.packages('tcltk')'",
                           "  --> If using a Mac, install latest XQuartz application (X11) from:",
                           "        https://www.xquartz.org/",
                           sep = "\n"))
  }

}
