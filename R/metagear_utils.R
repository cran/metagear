.metagearPROBLEM <- function(type, aMessage) {

  newMessage <- paste0("metagear ",
                       type,
                       " in ",
                       as.list(sys.call(-1))[[1]],
                       "(): ",
                       aMessage,
                       ".")
  if(type == "error") stop(newMessage, call. = FALSE)
  message(newMessage)

}

renameFile <- function(aFileName) {
  fileNumber <- gsub("[^0-9]", "", aFileName)
  aFileName <- ifelse(fileNumber == "",
                      gsub("\\.", "1.", aFileName),
                      gsub("\\d+", paste0(as.integer(fileNumber) + 1), aFileName))
  return(aFileName)
}

.metagearDependencies <- function(dependency) {

  # to do: generalize for other external packages
  if (!requireNamespace(dependency, quietly = TRUE)) {

    if(isTRUE(utils::askYesNo("Extracting data from figures with metagear requires the EBImage package, install now? \n\n NOTE: if YES, during installation choose option: a (to complete updates of EBImage).", FALSE))) {
      message("*****   Installing EBImage... it will take some time, but choose option: a (to complete)...")
      eval(parse(text = "install.packages(\"BiocManager\", repos = \"http://cran.us.r-project.org\", quiet = TRUE); BiocManager::install(\"EBImage\"); library(EBImage);"))
    } else {
      packageStartupMessage("*****   You can install and load later with this R script:")
      packageStartupMessage("             install.packages(\"BiocManager\", repos = \"http://cran.us.r-project.org\");")
      packageStartupMessage("             BiocManager::install(\"EBImage\");")
      packageStartupMessage("             library(EBImage)")

      .metagearPROBLEM("error", "Function disabled until EBImage package is installed.")
    }

    message("*****   EBImage installed and loaded.")
  }

}
