paintPoints <- function(aDetectedPlot, aBasePlot, size = 8, color = "blue") {
  
  blankImage <- EBImage::channel(EBImage::Image(dim = dim(aDetectedPlot)),  "rgb")
  theCoordinates <- EBImage::computeFeatures.moment(aDetectedPlot)[, 1:2]
  kern <- EBImage::makeBrush(size = size, shape = "disc", step = FALSE)
  median <- median(1:size) - 1 - 0.5
  
  for(a in 1:nrow(theCoordinates)) {
    corX <- (theCoordinates[a, 1] - median)
    corY <- (theCoordinates[a, 2] - median)
    blankImage[corX:(corX + size - 1 + 0.5), corY:(corY + size - 1), 1] <- kern
  }
    
  blankImage <- EBImage::channel(blankImage, "grey")
  
  theNew <- EBImage::paintObjects(blankImage, 
                         EBImage::channel(aBasePlot, "rgb"), 
                         col = rgb(t(col2rgb(color)), maxColorValue = 255), 
                         opac = 0.5,
                         thick = TRUE) 
  
  return(theNew)
}

unitTrim <- function(unit) {
  as.numeric(sub("native", "", as.character(unit)))
}

figureCutCoord <- function(theFig,
                           axis = "X",
                           sensitivity = 0.4, 
                           border = 5, 
                           top = 10) {
						   
  # get image dimensions
  xDim <- dim(theFig)[1]; yDim <- dim(theFig)[2];
  
  # find longest consecutive pixel rows with zeros and remove guesses 
  # near figure edges
  if(axis == "X") {
    guess <- rowSums(theFig[0:xDim, 0:yDim])
  } else {
    guess <- colSums(theFig[0:xDim, 0:yDim])
  }
  
  detected <- rle(guess)
  topGuesses <- tail(sort(detected$length), top)
  guesses <- which(detected$length >= (topGuesses[top] - topGuesses[top] * sensitivity))
  guesses <- guesses[which(detected$values[guesses] == 0)] # new line
  guesses <- guesses[which((guesses > border) & (guesses < length(detected$lengths) - border))]

  if(length(guesses) == 0) {
    .metagearPROBLEM("warning", 
					 paste0("no detected divisions among the sub-plots along the ", 
					        axis))
    return(NULL)
  }
  
  # get pixel coordinates
  theCoord <- guesses
  for(i in 1:length(guesses)) 
    theCoord[i] <- sum(detected$length[1:(guesses[i] - 1)]) + 
                                floor(detected$length[guesses[i]] / 2)

  return(theCoord)
}
