#' Redistributes title/abstract screening efforts among a review team.
#'
#' Randomly re-distributes screening tasks from one reviewers to the rest of the 
#' reviewing team.  Used when screening effort needs to be re-allocated among
#' reviewing team members.    
#'
#' @param aDataFrame A data.frame containing the titles and abstracts to be 
#'    screened by a team.  The default assumes that the data.frame has already 
#'    been formatted using \code{effort_initialize} and populated with 
#'    \code{effort_distribute}.
#' @param column_name Changes the default label of the "REVIEWERS" column 
#'    that contains the screening efforts of each team member. 
#' @param reviewer The name of the reviewer whose effort is to be redistributed. 
#' @param remove_effort The percentage of effort to be redistributed among the team.   
#'    The default is that 100\% of the effort will be re-distributed.
#' @param reviewers A vector of the names of each team member that will take on
#'    additional work.
#' @param effort A vector of percentages used to allocate screening
#'    tasks among each team member.  When not called explicitly, assumes effort 
#'    to be distributed evenly among all members.  Must be the same length as 
#'    the number of team members, and also sum to 100.  
#' @param save_split Saves the allocated team effort into separate "effort_*.csv"
#'    files for individual screening tasks.  These files can be given to each
#'    member to screen their random title/abstract subset.  All files can be 
#'    merged once all screening tasks have been completed using 
#'    \code{effort_merge}.  
#' @param directory Changes the default location/directory for where the  
#'    "effort_*.csv" will be saved.  If not explicitly called, it will deposit 
#'    files in the current working directory.
#'
#' @return A single data.frame with effort re-allocated among team members.
#'
#' @importFrom utils read.csv
#' @export effort_redistribute

effort_redistribute <- function (aDataFrame,
                                 column_name = "REVIEWERS",
                                 reviewer = NULL, 
                                 remove_effort = 100, 
                                 reviewers = NULL, 
                                 effort = NULL,
                                 save_split = FALSE,
                                 directory = getwd()) {
  
  
  if(remove_effort == 0) return(aDataFrame)
  if(is.null(reviewers)) .metagearPROBLEM("error", 
                                          "no reviewers were assigned")
  if(is.null(aDataFrame)) .metagearPROBLEM("error", 
                                           "a dataframe with refs was not specified")
  
  split_dataFrame <- split(aDataFrame, aDataFrame[, column_name])
  redist_dataFrame <- split_dataFrame[reviewer]
  split_dataFrame[reviewer] <- NULL
  
  if (length(reviewers) == 1) {
    .metagearPROBLEM("warning", 
                     "since reviewers had only one team member, all effort was distributed to them")
    new_dataFrame <- redist_dataFrame
    new_dataFrame[[1]][column_name] <- reviewers

  } else {
    theTeam <- reviewers
    number_reviewers <- length(theTeam)
    
    if(is.null(effort)) {
        teamEffort <- rep(ceiling(remove_effort/(number_reviewers - 1)), 
                          number_reviewers - 1)
        effort <- c(100 - remove_effort, teamEffort)
        if(sum(effort) > 100) 
          effort[number_reviewers] <- effort[number_reviewers] - (sum(effort) - 100)
    } 
  
    new_dataFrame <- list(effort_distribute(redist_dataFrame[[1]],
                                            theTeam,
                                            effort = effort,
                                            column_name = column_name,
                                            dual = FALSE,
                                            initialize = FALSE,
                                            save_split = FALSE))
      
  } 
    
  newDataFrame <- Reduce(function(...) rbind(...), c(split_dataFrame, new_dataFrame))
  
  if(save_split == TRUE) effort_save(newDataFrame, column_name, directory)
  
  return(newDataFrame)
}