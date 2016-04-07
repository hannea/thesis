# function that converts the timestamps, creates numerics when characters, and
# identifies the different WTs and returns a list with WTs as 
# dataframes with the same variables as the dataframe they came from.

split_WTs_func = function(dataframe, id = names(dataframe[1])){
  dataframe[["CMSTimeStamp"]] = as.POSIXct(dataframe[["CMSTimeStamp"]], tz = "GMT")
    
  split_data = split(dataframe,dataframe[[id]])

  n = length(split_test)
  names(split_data) = paste("WT", 1:n, sep = "")
  cat("Number of WTs = ", n, "\n")
  
  return(split_data)
}

#### test ####
# data_splitted = split_WTs_func(data)
# class(data_splitted) # list
# names(data_splitted)

# library(devtools)
# source_url("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
# list[WT1, b,c,d] = split_test