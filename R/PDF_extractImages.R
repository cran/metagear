#' Attempts to extract all images from a PDF
#'
#' Tries to extract images within a PDF file.  Currently does not support 
#' decoding of images in CCITT compression formats. However, will still save 
#' these images to file; as a record of the number of images detected in the PDF.
#'
#' @param file The file name and location of a PDF file.  Prompts
#'    for file name if none is explicitly called.  
#'    
#' @return A vector of file names saved as images.
#'
#'
#' @importFrom hexView readRaw blockValue
#' @importFrom tools file_path_sans_ext
#' @importFrom RCurl base64Decode
#' @export PDF_extractImages

PDF_extractImages <- function(file = file.choose()) {
  
  # check if file is a PDF
  if(isPDF(file, verbose = "quiet") != TRUE) {
    .metagearPROBLEM("error",
                     "file not PDF format")
  }
  
  # read file in HEX also ASCII
  rawFile <- readRaw(file, human = "char")
  
  # test if read file characters is same as file size
  if (length(blockValue(rawFile)) != file.info(file)$size) {
    .metagearPROBLEM("error",
                     "possible size reading error of PDF")
  }
  
  # extract images embeded as PDF objects
  createdFiles_bin <- scanPDFobjects(rawFile, file)
  #if(quiet != TRUE) message(paste0(createdFiles_bin), " ")

  # extract images embeded in XML
  createdFiles_XML <- scanPDFXML(rawFile, file)
  #if(quiet != TRUE) message(paste0(createdFiles_XML), " ")

  theSavedFileNames <- c(createdFiles_bin, createdFiles_XML)
    
  #print(round(7/3) + 7 %% 3)
  #if(ignore != TRUE) {
  #  
  #  par(mfrow = c(2,3), las = 1)
  #  for(i in 1:6) {
  #    figure_display(theSavedFileNames[i])
  #    mtext(theSavedFileNames[i], col = "red", cex = 1.2)
  #  }
  #}
  
  return(theSavedFileNames)
}

scanPDFobjects <- function (rawFile, file) {
  
  # collapse ASCII to a single string
  theStringFile <- paste(blockValue(rawFile), collapse = '')
  
  # split string by PDF objects and keep delimiter
  theObjects <- paste(unlist(strsplit(theStringFile, "endobj")), "endobj", sep="")
  
  # identify and screen candidate objects with images
  candidateObjects <- c(which(str_extract(theObjects, "XObject/Width") == "XObject/Width"), 
                        which(str_extract(theObjects, "Image") == "Image"))
  
  removeObjects <- c(which(str_extract(theObjects, "PDF/Text") == "PDF/Text"), 
                     which(str_extract(theObjects, "PDF /Text") == "PDF /Text"))
  

  candidateObjects <- unique(candidateObjects[! candidateObjects %in% removeObjects])

  if(length(candidateObjects) == 0) {
    return("No PDF image objects detected.")
  }
  
  # generate file names for candidate images
  fileNames <- paste(rep(file_path_sans_ext(file), length(candidateObjects)), 
                     "_bin_", 1:length(candidateObjects), ".jpg", sep="")
  
  # extract and save all image binaries found in PDF
  theNewFiles <- sapply(1:length(candidateObjects), 
                        function(x, y, z) PDFobjectToImageFile(y[x], 
                                                               theObjects, 
                                                               file, 
                                                               z[x]), 
                        y = candidateObjects, z = fileNames)
  
  return(theNewFiles)
}

PDFobjectToImageFile <- function (objectLocation, 
                                  theObjects, 
                                  theFile, 
                                  imageFileName) {
  
  # parse object by stream & endstream
  parsedImageObject <-  unlist(strsplit(theObjects[objectLocation], "stream"))

  # extract key char locations of image in PDF with trailingChars as a correction 
  # for "stream" being followed by 2 return characters 
  trailingChars <- "  "
  startImageLocation <- nchar(paste(parsedImageObject[1], 
                                    "stream", trailingChars, sep = ""))
  endImageLocation <- startImageLocation + 
                      nchar(substr(parsedImageObject[2], 
                                   1, 
                                   nchar(parsedImageObject[2]) - nchar("end")))
  PDFLocation <- nchar(paste(theObjects[1:(objectLocation - 1)], collapse = ''))
  
  # extract binary of image from PDF
  PDFImageBlock <- readRaw(theFile, 
                           offset = PDFLocation + startImageLocation, 
                           nbytes = endImageLocation, machine = "binary")
  
  # sometimes some of the orginal file format unicode is missing, this helps clean
  # this issue for jpgs at least
  if((PDFImageBlock$fileRaw[1] == "d8") && (PDFImageBlock$fileRaw[2] == "ff"))
      PDFImageBlock$fileRaw <- c(as.raw('0xff'), PDFImageBlock$fileRaw)
  
  # save binary of image to new file
  detectedImageFile <- file(imageFileName, "wb")
    writeBin(PDFImageBlock$fileRaw, detectedImageFile)
  close(detectedImageFile)
  
  # TO DO RETURN INFO ABOUT SUCCESSFUL FILE SAVE
  return(imageFileName)
}

scanPDFXML <- function (rawFile, file) {
  
  # collapse ASCII to a single string
  theStringFile <- paste(blockValue(rawFile), collapse = '')
  
  # split by XML tags with images and keep delimiter
  theObjects <- paste(unlist(strsplit(theStringFile, "xmpGImg:image>")),
                      "xmpGImg:image>", sep="")
  
  # identify objects with images
  candidateObjects <- which(str_extract(theObjects, "</xmpGImg:image>") == "</xmpGImg:image>")
  
  if(length(candidateObjects) == 0) {
    return("No XML image objects detected.")
  }
  
  # generate file names for candidate images
  fileNames <- paste(rep(file_path_sans_ext(file), length(candidateObjects)),
                     "_XML_", 1:length(candidateObjects), ".jpg", sep="")
  
  # extract and save all image binaries found in PDF
  theNewFiles <- sapply(1:length(candidateObjects), 
                        function(x, y, z) PDFXMLToImageFile(y[x], 
                                                            theObjects, 
                                                            file, 
                                                            z[x]), 
                        y = candidateObjects, z = fileNames)
  
  return(theNewFiles)
}

PDFXMLToImageFile <- function (objectLocation, 
                               theObjects, 
                               theFile, 
                               imageFileName) {
  
  # parse encoded XML image and clean
  parsedImage <- unlist(strsplit(theObjects[objectLocation], "</xmpGImg:image>"))
  parsedImage <- gsub("&#xA;", "", parsedImage[1])
  
  # decode image to base64
  decodedImage <- base64Decode(parsedImage, "raw")
  
  # save binary of image to new file
  detectedImageFile <- file(imageFileName, "wb")
    writeBin(decodedImage, detectedImageFile)
  close(detectedImageFile)
  
  # TO DO RETURN INFO ABOUT SUCCESSFUL FILE SAVE
  return(imageFileName)
}
