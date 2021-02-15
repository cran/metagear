#' Displays the detected figure objects.
#'
#' Generates a raster image of a figure with the detected objects painted on a 
#'    background/reference figure.
#'
#' @param aDetectedPlot A binary figure image with detected objects
#'    (an \code{EBImage} object).
#' @param background An \code{EBImage} figure of same size to be used as
#'    background (e.g., the original [RGB/color] figure image).
#' @param color The color to paint the detected objects.
#' @param ignore When \code{TRUE} does not display painted image, only 
#'    returns painted image EBImage object.
#'
#' @return A RGB \code{EBImage} painted with detected figure objects.
#' 
#' @importFrom grDevices rgb col2rgb
#' @export

figure_displayDetections <- function (aDetectedPlot,
                                      background = NULL,
                                      color = "red",
                                      ignore = FALSE) {
									  
  # if EBImage not installed, do it
  .metagearDependencies("EBImage")

  # overlay extractions onto background figure
  paintedFigure <- EBImage::paintObjects(aDetectedPlot, 
                                EBImage::channel(background, "rgb"), 
                                col = rgb(t(col2rgb(color)), maxColorValue = 255), 
                                opac = 0.75,
                                thick = TRUE)
  
  # returns a RGB EBimage object painted with detected objects
  if(!ignore) EBImage::display(paintedFigure, method = "raster")
  return(paintedFigure)
  
}