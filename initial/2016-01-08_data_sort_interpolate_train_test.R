#=========================================================================================
# Description: data saved as training and testing files
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
source("interpolate_func_Vestas_file.R")
interpolated = interpolate_func(sorted_data, var = "RMSISOGeneratorDE")

time = interpolated$times
y = interpolated[2:5]
ymatrix = do.call(rbind,y);            dim(ymatrix)       # 4 263490

################################################################################
## Define training period
################################################################################

# approx 2 months training period
two_months = 60*60*24*60
ii = time < two_months

time_train = time[ii];                 length(time_train) #   43200
vib_train  = t(subset(t(ymatrix),ii)); dim(vib_train)     # 4 43200

################################################################################
## Define test period
################################################################################
three_months = 60*60*24*90
jj = time > two_months & time < three_months

time_test = time[jj];                 length(time_test)  #   21600
vib_test  = t(subset(t(ymatrix),jj)); dim(vib_test)      # 4 21600

################################################################################
## Define faillure period
################################################################################
kk = time > three_months

time_failure = time[kk];                 length(time_failure)  #   198690
vib_failure  = t(subset(t(ymatrix),kk)); dim(vib_failure)      # 4 198690

# setwd("~/aspeciale/R_data_local")
# save(time_train,vib_train, time_test, vib_test,time_failure, vib_failure,file 
# = "train_test_failure.Rdata")
