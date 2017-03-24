#' Splits a composite figure that contains multiple plots.
#'
#' Automatically detects divisions among multiple plots found within a single
#' figure image file.  It then uses these divisions to split the image into 
#' multiple image files; each containing only a single X-Y plot.  Currently only
#' works on composite figures that have a matrix-style presentation where each 
#' sub-plot has the same size.       
#'
#' @param file The file name and location of a composite figure.  Prompts
#'    for file name if none is explicitly called.
#' @param binary_threshold A proportion from zero to one designating the 
#'    gray-scale threshold to convert pixels into black or white.  Pixel
#'    intensities below the proportion will be converted to black, and those 
#'    above white.
#' @param space_sensitivity_X A proportion ranging from zero to one that 
#'    designates the size of the separation among sub-plots along the X-axis 
#'    relative to the largest empty space detected in the figure image.  As
#'    space_sensitivity_X approaches 1, finer empty spaces (e.g., empty spaces
#'    found in between plot captions and the axis line) will be treated as plot
#'    divisions.
#' @param space_sensitivity_Y A proportion ranging from zero to one that 
#'    designates the size of the seperation among sub-plots along the Y-axis 
#'    relative to the largest empty space detected in the figure image.  As
#'    space_sensitivity_Y approaches 1, finer empty spaces (e.g., empty spaces
#'    found in between plot captions and the axis line) will be treated as plot
#'    divisions.
#' @param border_buffer An integer value designating the amount of empty space
#'    around the figure image that should be ignored.  As the number increases,
#'    more blank space near the image's edge will be ignored. 
#' @param guess_limit An integer value designating the number of guesses for 
#'    within a figure image.  The default value designates the top 10 guesses of 
#'    divisions.  Increase this number if there are more than 6 subplots per axis.   
#' @param ignoreX When \code{TRUE}, ignores detection of sub-plots along the 
#'    X-axis.
#' @param ignoreY When \code{TRUE}, ignores detection of sub-plots along the 
#'    Y-axis.
#' @param quiet When \code{TRUE}, does not print to console the saved file names.
#'
#' @return The number of sub-plots saved to file.
#'
#' @importFrom EBImage readImage
#' @export

figure_splitPlot <- function (file = file.choose(),
                              binary_threshold = 0.6,
							  space_sensitivity_X = 0.4,
							  space_sensitivity_Y = 0.6,
							  border_buffer = 5,
							  guess_limit = 10,
							  ignoreX = FALSE,
							  ignoreY = FALSE,
                              quiet = FALSE) {
  
  theFigure <- readImage(file)
  
  # load figure and convert to binary (searchable) format
  aBinaryFigure <- figure_transformToBinary(theFigure,
                                            binary_threshold)
											
											
  # get image dimensions & best guesses for splits
  xDim <- dim(aBinaryFigure)[1]
  if(ignoreX == TRUE) {
	theXtemp <- NULL
  }
  else {
    theXtemp <- figureCutCoord(theFig = aBinaryFigure,
                               axis = "X",
                               sensitivity = space_sensitivity_X,
                               border = border_buffer, 
                               top = guess_limit)
  }
  theX <- c(0, theXtemp,  xDim) 

  yDim <- dim(aBinaryFigure)[2];
  if(ignoreY == TRUE) {
	theYtemp <- NULL
  }
  else { 
    theYtemp <- figureCutCoord(theFig = aBinaryFigure,
                               axis = "Y",
                               sensitivity = space_sensitivity_Y, 
                               border = border_buffer, 
                               top = guess_limit)
  } 
  theY <- c(0, theYtemp, yDim)
  
  # save splits as separate images
  countFig <- 1
  totalFigs <- (length(theX) - 1) * (length(theY) - 1)
  if(totalFigs > 1) {
    for (i in 1:(length(theX) - 1)) {
      for (j in 1:(length(theY) - 1)) {
        croppedFig <- theFigure[theX[i]:theX[i + 1], theY[j]:theY[j + 1], ]
		newFileName <- paste0(file_path_sans_ext(file), "_subPlot_", countFig, "_of_", totalFigs, ".jpg")
        if(quiet != TRUE) print(newFileName)
        figure_write(croppedFig, file = newFileName)
        countFig <- countFig + 1
      }
    }
  } else {
     .metagearPROBLEM("warning", "sub-plots were not detected in this figure image")
  }

  return(totalFigs)  
}