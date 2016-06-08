#=========================================================================================
# Description: Different test of Kalman filter using dlm package and own implementation
#
# Load: training, testing and failure data
#=========================================================================================
setwd("~/aspeciale/R_data_local")
load("train_test_failure.Rdata")

################################################################################
## dlm with four dimensional vector
################################################################################
library(dlm)

build = function(parm){
  dlm(FF = as.matrix(rep(1,4)), V = diag(4)*parm[1], GG = 1, W = parm[2], m0 = 0, C0 = 1)
}

################################################################################
## parameter estimation by MLE (optim)
################################################################################
par_estimate = dlmMLE(vib_train, parm = c(0.01,0.01),build = build,lower=c(0,0))

par_estimate$convergence
par_estimate$value

parameters = par_estimate$par; parameters 
R = parameters[1]*diag(4)
Q = parameters[2]

################################################################################
## Kalman filter on test data
################################################################################
dlmmodel = build(parameters); dlmmodel
filter = dlmFilter(vib_test,dlmmodel, simplify = TRUE)
class(filter) # dlmFiltered

# filter$y == vib_test # think theese are identical # if simplify FALSE
head(filter$f); dim(filter$f) # 4 21600
class(filter$f) # matrix
length(time_test) # 21600
head(dropFirst(filter$m)); class(filter$m) # numeric
length(filter$m) # 21601

par(mfrow = c(3,1))
matplot(time_test,t(vib_test), pch = ".")
legend("topright", c("1", "2", "3", "4"), lty = c(1,1,1,1), col = c(1,2,3,4), cex = 0.6)
matplot(time_test,t(filter$f), pch = ".") # forecast of observations
plot(time_test,filter$m[-1], pch = ".") # starts one time unit before


matplot(time_test[1:5000], t(vib_test[,1:5000]), pch = ".")
matplot(time_failure, t(vib_failure), pch = ".")


matplot(time_test[1:2000], t(filter$f[,1:2000]), pch = ".")
points(time_test[1:500],filter$f[2:501], col = "red", lwd = 1) # or dropFirst

matplot(time_test[1:5000],filter$m[2:5001], pch = ".")

par(mfrow = c(1,1))
plot(filter$m[1:100], type="l",col="black") # m0 = 0
plot(filter$m[2:1000], type="l",col="black")
lines(filter$a[2:1000], col = "red", lwd = 1) # or dropFirst
legend("topright", c("filtered","predicted"), lwd = c(1,1), col = c("black","red"), cex = 0.8)

## same as for own implementation
plot(filter$D.C[1:1000]^2, type="l",col="black") # need to square to get SVD.
plot(filter$D.C[1:1000]^2, pch = 20, col="black", ylim = c(0.002,0.005))
plot(filter$D.C[1:1000]^2, pch = 20, col="black", ylim = c(0.0031,0.0034)) # need to square to get SVD.
# Unitary matrix is just 1.
points(filter$D.R[2:1001]^2, col = "red", pch = 20) # or dropFirst
legend("topright", c("filtered","predicted"), lwd = c(1,1), col = c("black","red"), cex = 0.8)

var = filter_values_spline$D.R^2
################################################################################
## Kalman filter using own implementation
################################################################################
setwd("C:/Users/Hanne/Dropbox/aspeciale/R/data_investigation/functions")
source("Kalman_filter_random_walk.R")

#time_POSIXct = as.POSIXct(time_test, tz = "GMT", origin = "2012-01-01")
R;Q
#KalmanFilter_test = KalmanFilterRandomWalk(vib_test, R = R, Q = Q)
KalmanFilter_test = Kalman_filter_random_walk(vib_test, R = R, Q = Q)
par(mfrow = c(2,2))
#plot(time_POSIXct, KalmanFilter_test$state_covariances[-1], pch = ".", type = "l", ylim = c(0,1))
kk = 1:100
time_test_POSIXct = as.POSIXct(time_test, tz = "gmt", origin = "2013-01-01")
plot(time_test_POSIXct[kk],KalmanFilter_test$state_covariances[kk], pch = 20, 
     ylim = c(0,1.2),  xlab = "time", ylab = "variance")
plot(time_test_POSIXct[kk],KalmanFilter_test$state_covariances[kk], pch = 20, 
     ylim = c(0.003,0.005),  xlab = "time", ylab = "variance")

plot(KalmanFilter_test$state_covariances, pch = ".", ylim = c(0,0.3))
plot(KalmanFilter_test$state_covariances, pch = ".", ylim = c(0,0.01))
plot(KalmanFilter_test$state_covariances, pch = ".", ylim = c(0.003,0.005))
plot(KalmanFilter_test$state_covariances, pch = ".", ylim = c(0.0031,0.0034))

par(mfrow = c(2,1), mar = c(1,4,1,1))
kk = 1:3000
kk = 4000:6000
kk = 1:21000
kk = 8800:11200 ## This one!!
kk = 1300:2600
matplot(time_test_POSIXct[kk],t(vib_test[,kk]), pch = ".", xlab = "time", ylab = "y_t")
plot(time_test_POSIXct[kk],KalmanFilter_test$state_values[kk], pch = ".", xlab = "time", ylab = "x_t")

#######################################################
vib_df = data.frame(t(vib_test[,kk]))
names(vib_df) = c("WT1", "WT2", "WT3", "WT4")
df = cbind(time = time_test_POSIXct[kk], vib_df, state = KalmanFilter_test$state_values[kk])
library(tidyr)
df_tidy = gather(df, "name", "vibration", 2:6)
head(df_tidy)

library(RColorBrewer)
myColors = brewer.pal(5,"Set1")
names(myColors) = levels(df_tidy$name)
colScale = scale_colour_manual(name = "signal",values = myColors)

library(ggplot2)
ggplot(df_tidy, aes(time, vibration, col=name)) + geom_line()  +
  geom_line(data = subset(df_tidy, name == "state"),  size = 2) + colScale



setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("rw_filter.png", width = 6.5, height = 3)
################################################################################
## Compare to not interpolated data
################################################################################
setwd("~/aspeciale/R_data_local")
load("4WT_1Month.RData") 
setwd("C:/Users/Hanne/Dropbox/aspeciale/R/data_investigation/functions")
source("identify_WTs_func_Vestas_file.R")
identified_data = identify_WTs_func(data)

startStamp = as.POSIXct(min(identified_data[[1]][["CMSTimeStamp"]]))
time_train_POSIXct = as.POSIXct(time_train, tz = "GMT", origin = startStamp)
time_test_POSIXct = as.POSIXct(time_test, tz = "GMT", origin = startStamp)
matplot(time_train_POSIXct[21500:22000], t(vib_train[,21500:22000]), pch = ".")
plot(time_train_POSIXct[1:5000], t(vib_train[1,1:5000]), pch = ".")



setwd("~/aspeciale/R_data_local")
load("4WT_1Year_1Failure.RData") 
setwd("C:/Users/Hanne/Dropbox/aspeciale/R/data_investigation/functions")
source("identify_WTs_func_Vestas_file.R")
sorted_data = identify_WTs_func(data)

time = list(sorted_data$WT1$CMSTimeStamp[1:50000],sorted_data$WT2$CMSTimeStamp[1:50000], 
            sorted_data$WT3$CMSTimeStamp[1:50000], sorted_data$WT4$CMSTimeStamp[1:50000])
time_matrix = do.call(rbind,time);    dim(time_matrix)
y_before = list(sorted_data$WT1$RMSISOGeneratorDE[1:50000],sorted_data$WT2$RMSISOGeneratorDE[1:50000], 
                sorted_data$WT3$RMSISOGeneratorDE[1:50000], sorted_data$WT4$RMSISOGeneratorDE[1:50000])
y_before_matrix = do.call(rbind,y_before);            dim(y_before_matrix)       # 4 263490

matplot(t(time_matrix),t(y_before_matrix), pch = ".")
plot(time[[1]],y_before[[1]], pch = ".")
################################################################################
## succesful comparison of what is wrong with interpolation
################################################################################
par(mfrow = c(2,1))
plot(time[[1]][37606:41292],y_before[[1]][37606:41292], pch = ".", xlab = "time", 
     ylab = "vib")
plot(time_test_POSIXct[1:5000], t(vib_test[1,1:5000]), pch = ".", xlab = "time", 
     ylab = "vib interpolated")

df = data.frame(time = time[[1]][37606:41292], vibration = y_before[[1]][37606:41292], 
                method = "none")
df = rbind(df, data.frame(time = (time_test_POSIXct[1:5000] - 365*24*3600), vibration = vib_test[1,1:5000], 
                          method = "interpolated"))

library(ggplot2)
ggplot(df, aes(time, vibration)) + geom_point(size = 0.1) + facet_wrap(~method, ncol = 1)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("interpolation_and_none.png", width = 6.5, height = 3.7)

plot(time_test_POSIXct[3000:4000], t(vib_test[1,3000:4000]), pch = ".", xlab = "time", 
     ylab = "vib interpolated")

################################################################################
## Kalman filter on faillure set
################################################################################
KalmanFilter_failure = KalmanFilterRandomWalk(vib_failure)
par(mfrow = c(2,2))
plot(KalmanFilter_failure$state_covariances, pch = ".", type = "l", ylim = c(0,1))
plot(KalmanFilter_failure$state_covariances, pch = ".", type = "l", ylim = c(0,0.5))
plot(KalmanFilter_failure$state_covariances, pch = ".", type = "l", ylim = c(0,0.05))
plot(KalmanFilter_failure$state_covariances, pch = ".", type = "l", ylim = c(0.0205,0.021))

par(mfrow = c(2,1))
matplot(time_failure,t(vib_failure), pch = ".")
legend("topright", c("1", "2", "3", "4"), lty = c(1,1,1,1), col = c(1,2,3,4), cex = 0.6)
plot(KalmanFilter_failure$state_values, pch = ".")
