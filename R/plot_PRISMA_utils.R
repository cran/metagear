# generic constructor for PRISMA phase box
phaseGrob <- function(aLabel, 
                      x = 0.5, 
                      y = 0.5, 
                      width = 25,
                      gp = NULL,
                      vp = NULL) {
                      
  theLabel <- strwrap(aLabel, width)
  nLabel <- length(theLabel) 
  phaseHeight <- unit(nLabel + 2, "lines")

  aBox <- roundrectGrob(x = x, 
                        y = y, 
                        width = max(stringWidth(theLabel)) + unit(12, "mm"), 
                        height = phaseHeight)

  centerTextAdjustment <- convertY(phaseHeight, "npc", TRUE) / 2.0 - 
                            convertY(unit(nLabel:1 + 0.5, "lines"), "npc", TRUE)

  theText <- textGrob(theLabel, x = x, y = y - centerTextAdjustment)  
  
  return(gTree(children = gList(aBox, theText), name = "aPhase"))
}

# change coordinates of phaseGrob by manipulating viewport
movePhaseGrob <- function(aGrob,
                        x = 0.5, 
                        y = 0.5) {
                      
    return(editGrob(aGrob, vp = viewport(x = x, y = y)))
}

# generates all the phases with no X or Y specified
getPhaseGrobs <- function(aPhaseVector, 
                          colWidth = 25) {
                          
  phaseList <- gList()
  for(aPhase in 1:length(aPhaseVector)) {
    phaseList <- gList(phaseList, 
                       phaseGrob(aPhaseVector[aPhase], width = colWidth)) 
  }
  return(phaseList)
}

# constructor for arrows that link downward flow phases
connectPhases <- function (parentPhase, 
                           childPhase,
                           allPhases) {
  
  parentHeight <- convertWidth(parentPhase$children[[1]]$height, "mm", TRUE) 
  childHeight <- convertWidth(childPhase$children[[1]]$height, "mm", TRUE)
  
  allPhases <- gList(allPhases, 
                     moveToGrob(grobX(parentPhase, "north"), 
                                grobY(parentPhase, "south") - unit(parentHeight / 2.0, "mm") ),
                     lineToGrob(grobX(childPhase, "south"), 
                                grobY(childPhase, "north") + unit(0.2, "mm") + unit(childHeight / 2.0, "mm"), 
                                arrow = arrow(type = "closed", length = unit(4, "mm")), 
                                gp = gpar(fill = "black")))
  return(allPhases)
}

# constructor for double arrows that link start phase to a single daughter phase
marryPhases <- function (parentPhaseLeft, 
                         childPhase, 
                         parentPhaseRight,
                         allPhases) {

  parentLeftHeight <- convertWidth(parentPhaseLeft$children[[1]]$height, "mm", TRUE) 
  childHeight <- convertWidth(childPhase$children[[1]]$height, "mm", TRUE)
  parentRightHeight <- convertWidth(parentPhaseRight$children[[1]]$height, "mm", TRUE)   
                         
  allPhases <- gList(allPhases, 
                     curveGrob(grobX(parentPhaseLeft, "north"),
                               grobY(parentPhaseLeft, "south") - unit(parentLeftHeight / 2.0, "mm"),
                               grobX(childPhase, "south") - unit(5, "mm"),
                               grobY(childPhase, "north") + unit(childHeight / 2.0, "mm"),
                               inflect = TRUE,
                               arrow = arrow(type = "closed",
                                             angle = 30,
                                             length = unit(4, "mm")),
                               gp = gpar(fill = "black", lwd = 1)),
                    curveGrob(grobX(parentPhaseRight, "north"),
                              grobY(parentPhaseRight, "south") - unit(parentRightHeight / 2.0, "mm"),
                              grobX(childPhase, "south") + unit(5, "mm"),
                              grobY(childPhase, "north") + unit(childHeight / 2.0, "mm"),
                              inflect = TRUE, curvature = -1,
                              arrow = arrow(type = "closed",
                                            angle = 30,
                                            length = unit(4, "mm")),
                              gp = gpar(fill = "black", lwd = 1)))
  return(allPhases)
}

# constructor for arrow that links exclusion (rightmost) phases
excludePhase <- function (parentPhase, 
                          exludedPhase, 
                          allPhases) {
                          
  parentWidth <- convertWidth(parentPhase$children[[1]]$width, "mm", TRUE) 
  excludeWidth <- convertWidth(exludedPhase$children[[1]]$width, "mm", TRUE)
  
  allPhases <- gList(allPhases, 
                     moveToGrob(grobX(parentPhase, "east")  + unit(parentWidth / 2.0, "mm"), 
                                grobY(parentPhase, "east")),
                     lineToGrob(grobX(exludedPhase, "west") - unit(excludeWidth / 2.0, "mm"), 
                                grobY(exludedPhase, "east"), 
                                arrow = arrow(type = "closed", 
                                        length = unit(4, "mm")), 
                                gp = gpar(fill = "black")))
  return(allPhases)
}

# generate simplified phase scheme
getPhaseScheme <- function(aPhaseVector) {
  aScheme <- lapply(aPhaseVector, function(x) {
    if(grepl("START_PHASE: ", x)) return("S") 
    if(grepl("EXCLUDE_PHASE: ", x)) return("E")
    return("P")
  })
  return(unlist(aScheme))
}

# generate main-line phase scheme (used to gauge length of flow chart)
getMainScheme <- function(aScheme) {
  mainScheme <- c(aScheme[1], aScheme[!(aScheme %in% "S" | aScheme %in% "E")])
  return (mainScheme)
}

# extracts phase labels without phase-definitions
getPhaseClean <- function(aPhaseVector) {
  aCleanVector <- lapply(aPhaseVector, function(x) {
    if(grepl("START_PHASE: ", x)) return(sub("START_PHASE: ", "", x)) 
    if(grepl("EXCLUDE_PHASE: ", x)) return(sub("EXCLUDE_PHASE: ", "", x))
    return(x)
  })
  return(unlist(aCleanVector))
}