# function that converts the timestamps, creates numerics when characters, and
# identifies the different WTs and returns a list with WTs as 
# dataframes with the same variables as the dataframe they came from.

## Roxygen....

#' Identify WTs in a Vestas file
#'
#' @param dataframe A CMS Vestas file as a data frame
#' @param id The string name of the column with WT id in it
#' @return The times converted to POSIXct and sorted by \code{id} into the different WTs from \code{dataframe}.
#' @examples
#' identify_WTs_func(dataframe, names(dataframe[1]))

identify_WTs_func = function(dataframe, id = names(dataframe[1])){
  dataframe[["CMSTimeStamp"]] = as.POSIXct(dataframe[["CMSTimeStamp"]], tz = "GMT")

  out = split(dataframe,dataframe[[id]])
  n = length(out)
  
  names(out) = paste("WT", 1:n, sep = "")
  cat("Number of WTs = ", n, "\n")
  return(out)
}