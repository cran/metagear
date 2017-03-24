#' Plots and creates a PRISMA flow diagram.
#'
#' Creates a PRISMA (Preferred Reporting Items for Systematic Reviews and 
#'    Meta-Analyses) flow diagram depicting the 'flow' of study inclusions and
#'    exclusions during various systematic review phases.  It is meant to 
#'    describe the number of studies identified, included, excluded, reasons
#'    for inclusion/exclusions, and final number of studies used in the
#'    meta-analysis.  NOTE: currently only supports two start phases, and final
#'    phase must not have an exclude branch.       
#'
#' @param aPhaseVector A vector of ordered labels (strings) for each phase of
#'    the PRISMA diagram.  Labels designating the beginning of the diagram
#'    are commented with "START_PHASE: " and those designating exclusion phases
#'    "EXCLUDE_PHASE: ".  These comments will be removed from the diagram.   
#' @param colWidth An optional value (integer) designating the width of the text 
#'    box of each phase.  
#' @param excludeDistance An optional value designating the the distance of 
#'    exclude phase box from the main flow diagram.  Larger values (> 0.8) 
#'    increase this distance.
#' @param design Designates the colorscheme and design of the the flow diagram.
#'    The default is \code{classic} (as in versions of metagear prior to v. 0.4).
#'    Others schemes are also available with color and more flat designs, and these
#'    can be further customized; see NOTE below for these details.       
#' @param hide When FALSE, the PRISMA flow diagram is not plotted.
#'
#' @return a grid object (grob) list 
#'
#' @examples
#' phases <- c("START_PHASE: # of studies identified through database searching",
#'             "START_PHASE: # of additional studies identified through other sources",
#'             "# of studies after duplicates removed",
#'             "# of studies with title and abstract screened",
#'             "EXCLUDE_PHASE: # of studies excluded",
#'             "# of full-text articles assessed for eligibility",
#'             "EXCLUDE_PHASE: # of full-text excluded, not fitting eligibility criteria",
#'             "# of studies included in qualitative synthesis",
#'             "EXCLUDE_PHASE: # studies excluded, incomplete data reported",
#'             "final # of studies included in quantitative synthesis (meta-analysis)")
#' plot_PRISMA(phases, design = "cinnamonMint")
#'
#' @note \strong{Using canned or custom PRISMA design layouts}\cr\cr There are 
#'    several color schemes and design layouts (e.g. curved or flat) available. 
#'    These designs include: \code{cinnamonMint}, \code{sunSplash}, \code{pomegranate},
#'    \code{vintage}, \code{grey}, and \code{greyMono}. Custom schemes can also 
#'    be developed by modifying each aspect of the design.  These are:
#'    \describe{
#'        \item{S}{color of start phases (default: white)}
#'        \item{P}{color of the main phases (default: white)}
#'        \item{E}{color of the exclusion phases (default: white)}
#'        \item{F}{color of the final phase (default: white)}
#'        \item{fontSize}{the size of the font (default: 12) }
#'        \item{fontColor}{the font color (default: black)}
#'        \item{fontFace}{either plain, bold, italic, or bold.italic (default: plain)}
#'        \item{flatArrow}{arrows curved when FALSE (default); arrows square when TRUE}
#'        \item{flatBox}{boxes curved when FALSE (default); Boxes square when TRUE}
#'    } 
#'    For example, changing the defaults to have red rather than white exclusion 
#'    phases, and square boxes, would be: \code{design = c(E = "red", flatBox = TRUE)}.
#'
#' @references Moher, D., Liberati, A., Tetzlaff, J. and Altman, D.G.,
#'    PRISMA Group. (2009) Preferred reporting items for systematic reviews and
#'    meta-analyses: the PRISMA statement. BMJ 339, b2535.
#'
#' @import grid
#' @export plot_PRISMA

plot_PRISMA <- function (aPhaseVector, 
                         colWidth = 30,
                         excludeDistance = 0.8,
                         design = "classic",
                         hide = FALSE) {
  
  grid.newpage(recording = FALSE)
  PRISMA_vp <- viewport(width = 1, height = 1)
  pushViewport(PRISMA_vp)
  
  if(is.null(names(design)) == TRUE) {
    prismaDesign <- designList[[design]]
    if(is.null(prismaDesign)) {
      .metagearPROBLEM("warning", "not an existing design scheme")
      # revert to default design
      prismaDesign <- aDesign()
    }
  } else {
    prismaDesign <- do.call(aDesign, as.list(design))
  }
 
  # generate all PRISMA phases with undefined x and y positions
  theGrobs <- getPhaseGrobs(getPhaseClean(aPhaseVector),
                            getPhaseScheme(aPhaseVector),
                            prismaDesign,
                            colWidth)

  phaseScheme <- getPhaseScheme(aPhaseVector)
  startPhases <- phaseScheme %in% "S"
  startIndex <- which(startPhases)
  endIndex <- which(!startPhases)
  
  xDistance <- round(1.0 / (length(startIndex) + 1.0), 2)
  xCoordinates <- xDistance
  yDistance <- round(0.8 / (length(getMainScheme(phaseScheme)) - 1.0), 2) 
  yCoordinates <- 0.9
  
  # plot (multiple) start phases on same horizontal line
  for(aPhase in startIndex) {
    theGrobs[[aPhase]] <- movePhaseGrob(theGrobs[[aPhase]], 
                                        x = xCoordinates, 
                                        y = yCoordinates)
    xCoordinates <- xCoordinates + xDistance
  }

  exclude <- FALSE
  lastBox <- theGrobs[[endIndex[1]]]
  if(length(startIndex) == 1) xDistance <- round(1.0 / 3.0, 2)
  
  # plot remaining PRISMA phases and link with arrows
  for(aPhase in endIndex) {
    if(yCoordinates == 0.9) {
    
      yCoordinates <- yCoordinates - yDistance
      lastBox <- movePhaseGrob(lastBox, y = yCoordinates)
      if(length(startIndex) > 1) {
        theGrobs <- marryPhases(theGrobs[[1]], 
                                lastBox, 
                                theGrobs[[2]], 
                                prismaDesign, 
                                theGrobs)
      } else {
        theGrobs <- connectPhases(theGrobs[[1]], 
                                  lastBox, 
                                  prismaDesign, 
                                  theGrobs)
      }
      theGrobs[[endIndex[1]]] <- lastBox
      
    } else {
    
      if (phaseScheme[aPhase] == "E") {
        theGrobs[[aPhase]] <- movePhaseGrob(theGrobs[[aPhase]], 
                                            x = round(0.5 + xDistance * excludeDistance, 2), 
                                            y = yCoordinates)
        exclude <- TRUE
      } else {
        theGrobs[[aPhase]] <- movePhaseGrob(theGrobs[[aPhase]], 
                                            y = yCoordinates - yDistance)
      }
      
      if (exclude == TRUE) {
        theGrobs <- excludePhase(lastBox, 
                                 theGrobs[[aPhase]], 
                                 prismaDesign, 
                                 theGrobs)
        exclude <- FALSE
      } else {
        theGrobs <- connectPhases(lastBox, 
                                  theGrobs[[aPhase]], 
                                  prismaDesign, 
                                  theGrobs)
        yCoordinates <- yCoordinates - yDistance
        lastBox <- theGrobs[[aPhase]]
      }
      
    }
  }

  # draw all the grid objects
  if (hide == FALSE) grid.draw(theGrobs, recording = FALSE)
    
  popViewport()
  return(theGrobs)  
}
