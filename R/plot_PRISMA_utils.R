# generic constructor for PRISMA phase box
phaseGrob <- function(aLabel, 
                      x = 0.5, 
                      y = 0.5, 
                      width = 25,
                      gp = NULL,
                      vp = NULL,
                      fontSize = 12,
                      color = "white", 
                      fontColor = "black",
                      fontFace = "plain",
                      flatBox = FALSE) {
                      
  theLabel <- strwrap(aLabel, width)
  nLabel <- length(theLabel) 
  phaseHeight <- unit(nLabel + 2, "lines")

  if(flatBox == TRUE) {
    rad <- unit(0.0, "snpc")
  } else {
    rad <- unit(0.1, "snpc")
  }
  
  if(color == "white") {
    borderCol <- "black"
  } else {
    borderCol <- color
  }

  aBox <- roundrectGrob(name = "thePhase", 
                        x = x, 
                        y = y, 
                        width = max(stringWidth(theLabel)) + unit(12, "mm"), 
                        height = phaseHeight,
                        r = rad,
                        gp = gpar(col = rgb(t(col2rgb(borderCol)), maxColorValue = 255), 
                                  fill = rgb(t(col2rgb(color)), maxColorValue = 255)))

  centerTextAdjustment <- convertY(phaseHeight, "npc", TRUE) / 2.0 - 
                            convertY(unit(nLabel:1 + 0.5, "lines"), "npc", TRUE)

  theText <- textGrob(theLabel, 
                      x = x, 
                      y = y - centerTextAdjustment,
                      gp = gpar(col = rgb(t(col2rgb(fontColor)), maxColorValue = 255), 
                                fontsize = fontSize, 
                                fontface = fontFace))  
  
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
                          aPhaseScheme,
                          prismaDesign,
                          colWidth = 25) {
                          
  phaseList <- gList()
  for(aPhase in 1:length(aPhaseVector)) {
 
    aColor <- ifelse(aPhase == length(aPhaseVector), 
                     prismaDesign["F"], 
                     prismaDesign[aPhaseScheme[aPhase]])
    
    phaseList <- gList(phaseList, 
                       phaseGrob(aPhaseVector[aPhase], 
                                 width = colWidth,
                                 fontColor = prismaDesign["fontColor"],
                                 fontSize = prismaDesign["fontSize"],
                                 fontFace = prismaDesign["fontFace"],
                                 flatBox = prismaDesign["flatBox"],
                                 color = aColor)) 
  }
  return(phaseList)
}

# constructor for arrows that link downward flow phases
connectPhases <- function (parentPhase, 
                           childPhase,
                           prismaDesign,
                           allPhases) {
  
  parentHeight <- convertWidth(parentPhase$children[[1]]$height, "mm", TRUE) 
  childHeight <- convertWidth(childPhase$children[[1]]$height, "mm", TRUE)
  
  
  if(prismaDesign["flatArrow"] == TRUE) {
    arrowSpace <- unit(2.5, "mm")
  } else {
    arrowSpace <- unit(0.0, "mm")
  }
  
  allPhases <- gList(allPhases, 
                     moveToGrob(grobX(parentPhase, "north") , 
                                grobY(parentPhase, "south") - 
                                  unit(parentHeight / 2.0, "mm") - arrowSpace),
                     lineToGrob(grobX(childPhase, "south"), 
                                grobY(childPhase, "north") + unit(0.2, "mm") + 
                                  unit(childHeight / 2.0, "mm") + arrowSpace, 
                                arrow = arrow(type = "closed",
                                              length = unit(4, "mm")),
                                gp = gpar(fill = "black", lwd = 1)) )
  return(allPhases)
}

# constructor for double arrows that link start phase to a single daughter phase
marryPhases <- function (parentPhaseLeft, 
                         childPhase, 
                         parentPhaseRight,
                         prismaDesign,
                         allPhases) {

  parentLeftHeight <- convertWidth(parentPhaseLeft$children[[1]]$height, "mm", TRUE) 
  childHeight <- convertWidth(childPhase$children[[1]]$height, "mm", TRUE)
  parentRightHeight <- convertWidth(parentPhaseRight$children[[1]]$height, "mm", TRUE)   
  
  if(prismaDesign["flatArrow"] == TRUE) {
    arrowSpace <- unit(2.5, "mm")
    arrowCurve <- 0.0
  } else {
    arrowSpace <- unit(0.0, "mm")
    arrowCurve <- 0.5
  }
  
  allPhases <- gList(allPhases, 
                     curveGrob(grobX(parentPhaseLeft, "north") ,
                               grobY(parentPhaseLeft, "south") - 
                                 unit(parentLeftHeight / 2.0, "mm") - arrowSpace,
                               grobX(childPhase, "south") - unit(5, "mm"),
                               grobY(childPhase, "north") + 
                                 unit(childHeight / 2.0, "mm") + arrowSpace,
                               inflect = TRUE, shape = arrowCurve,
                               arrow = arrow(type = "closed",
                                             length = unit(4, "mm")),
                               gp = gpar(fill = "black", lwd = 1)),
                    curveGrob(grobX(parentPhaseRight, "north"),
                              grobY(parentPhaseRight, "south") -
                                unit(parentRightHeight / 2.0, "mm") - arrowSpace,
                              grobX(childPhase, "south") + unit(5, "mm"),
                              grobY(childPhase, "north") + 
                                unit(childHeight / 2.0, "mm") + arrowSpace,
                              inflect = TRUE, curvature = -1, shape = arrowCurve,
                              arrow = arrow(type = "closed",
                                            length = unit(4, "mm")),
                              gp = gpar(fill = "black", lwd = 1)) )
  return(allPhases)
}

# constructor for arrow that links exclusion (rightmost) phases
excludePhase <- function (parentPhase, 
                          exludedPhase, 
                          prismaDesign,
                          allPhases) {
                          
  parentWidth <- convertWidth(parentPhase$children[[1]]$width, "mm", TRUE) 
  excludeWidth <- convertWidth(exludedPhase$children[[1]]$width, "mm", TRUE)
  
  if(prismaDesign["flatArrow"] == TRUE) {
    arrowSpace <- unit(2.5, "mm")
  } else {
    arrowSpace <- unit(0.0, "mm")
  }
  
  allPhases <- gList(allPhases, 
                     moveToGrob(grobX(parentPhase, "east")  + 
                                  unit(parentWidth / 2.0, "mm") + arrowSpace, 
                                grobY(parentPhase, "east") ),
                     lineToGrob(grobX(exludedPhase, "west") - 
                                  unit(excludeWidth / 2.0, "mm") - arrowSpace, 
                                grobY(exludedPhase, "east") , 
                                arrow = arrow(type = "closed", 
                                        length = unit(4, "mm")), 
                                gp = gpar(fill = "black", lwd=1))
                     )
  
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

# generic constructor for PRISMA design layouts
aDesign <- function(S = "white",
                    P = "white",
                    E = "white",
                    F = "white",
                    fontColor = "black",
                    fontSize = "12",
                    fontFace = "plain",
                    flatArrow = FALSE,
                    flatBox = FALSE) {
  
  list("S" = S,
       "P" = P,
       "E" = E,
       "F" = F,
       "fontColor" = fontColor,
       "fontSize" = fontSize,
       "fontFace" = fontFace,
       "flatArrow" = flatArrow,
       "flatBox" = flatBox
  ) 
}

# a list of PRISMA design schemes
designList <- list(

  classic         = aDesign(),
  
  cinnamonMint    = aDesign(S = "#f27649",
                            P = "#f29d3f",
                            E = "#f2c12e",
                            F = "#14a697",
                            fontColor = "white",
                            fontSize = "11",
                            fontFace = "bold",
                            flatArrow = TRUE,
                            flatBox = TRUE),
  
  sunSplash       = aDesign(S = "#42826C",
                            P = "#A5C77F",
                            E = "#C84663",
                            F = "#FFC861",
                            fontColor = "white",
                            fontSize = "11",
                            fontFace = "bold",
                            flatArrow = TRUE,
                            flatBox = TRUE),
  
  pomegranate     = aDesign(S = "#36B1BF",
                            P = "#4AD9D9",
                            E = "#C84663",
                            F = "#F5A503",
                            fontColor = "white",
                            fontSize = "11",
                            fontFace = "bold",
                            flatArrow = TRUE,
                            flatBox = TRUE),
  
  vintage         = aDesign(S = "#FF3D7F",
                            P = "#FF9E9D",
                            E = "#DAD8A7",
                            F = "#3FB8AF",
                            fontColor = "white",
                            fontSize = "11",
                            fontFace = "bold",
                            flatArrow = TRUE,
                            flatBox = TRUE),
  
  grey            = aDesign(S = "grey70",
                            P = "grey82",
                            E = "grey89",
                            F = "grey95",
                            fontColor = "grey25",
                            fontSize = "11",
                            fontFace = "bold",
                            flatArrow = TRUE,
                            flatBox = TRUE),  
  
  greyMono        = aDesign(S = "grey90",
                            P = "grey90",
                            E = "grey90",
                            F = "grey90",
                            fontColor = "grey25",
                            fontSize = "11",
                            fontFace = "bold",
                            flatArrow = TRUE,
                            flatBox = TRUE)  
  
)

