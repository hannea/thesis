#=========================================================================================
# Description: data saved as training and testing files, handling POSIXct class
#
# Load: "4WT_1Year_1Failure.RData"
# Save: training, testing and failure of data
#=========================================================================================
setwd("~/aspeciale/R_data_local")
load("4WT_1Year_1Failure.RData") 
setwd("C:/Users/Hanne/Dropbox/aspeciale/R/data_investigation/functions")

################################################################################
## sort the data
################################################################################
source("identify_WTs_func_Vestas_file.R")
sorted_data = identify_WTs_func(data)

################################################################################
## interpolate
################################################################################
source("interpolate_func_Vestas_file_POSIXct_NAs.R")
interpolated_vib = data.frame(interpolate_POSIXct_2mins(sorted_data, var = "RMSISOGeneratorDE"))
interpolated_load = interpolate_POSIXct_2mins(sorted_data, var = "GeneratorSpeed")

time = interpolated_vib$time_stamps
sum(time != interpolated_load$time_stamps) # they are all the same

library(ggplot2)
library(dplyr)
names(interpolated_vib)
names(interpolated_load)
interpolated_vib %>% ggplot(aes(time_stamps,RMSISOGeneratorDE_spline1)) +
  geom_point()

#par(mfrow = c(1,1))
#plot(interpolated_load$time_stamps, interpolated_load$GeneratorSpeed_spline1, pch = ".")
#plot(sorted_data$WT1$CMSTimeStamp, sorted_data$WT1$RMSISOGeneratorDE, pch = ".")

vib = interpolated_vib %>% select(contains("spline"))
load = interpolated_load %>% select(contains("spline"))

head(vib)
dim(vib)
dim(interpolated_vib)
################################################################################
## Define training period
################################################################################

# approx 2 months training period
two_months = time[1] + 60*60*24*60
ii = time < two_months

time_train = time[ii];                  length(time_train) # 43200
vib_train  = subset(vib,ii);            dim(vib_train)     # 43200 4
load_train = subset(load,ii);           dim(load_train)    # 43200 4

################################################################################
## Define test period
################################################################################
three_months = time[1] + 60*60*24*90
jj = time > two_months & time < three_months

time_test = time[jj];                length(time_test)   # 21599
vib_test  = subset(vib,jj);          dim(vib_test)       # 21599 4
load_test = subset(load,jj);         dim(load_test)      # 21599 4

################################################################################
## Define faillure period
################################################################################
kk = time > three_months

time_failure = time[kk];                 length(time_failure)  # 198690
vib_failure  = subset(vib,kk);           dim(vib_failure)      # 198690 4
load_failure = subset(load,kk);          dim(load_failure)     # 198690 4

#  setwd("~/aspeciale/R_data_local")
#  save(time_train, vib_train, load_train, 
#       time_test, vib_test, load_test,
#       time_failure, vib_failure, load_failure,
#       file = "train_test_failure_approx_dplyr.Rdata")
