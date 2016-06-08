#=========================================================================================
# Description: Investigating the residual process
#
# Load: training, testing and failure data
#=========================================================================================
################################################################################
## Residual process 
################################################################################
## Load train_test_failure interpolated large data set
################################################################################
setwd("~/aspeciale/R_data_local")
load("train_test_failure_approx_dplyr.Rdata")
class(load_train) # data.frame
names(load_train)
head(load_train)

library(ggplot2)
load_train %>% ggplot(aes(time_train,RMSISOGeneratorDE_spline1)) +
  geom_point()

plot(load_train[[1]],vib_train[[1]], main = "Train set",
     pch = 20, col = rgb(0,0,0,0.02), xlab = "Load", ylab = "Vibrations")
plot(load_test[[1]],vib_test[[1]], main = "Test set",
     pch = 20, col = rgb(0,0,0,0.02), xlab = "Load", ylab = "Vibrations")

setwd("C:/Users/Hanne/Dropbox/aspeciale/R/data_investigation/functions")
source("count_alarms_constant_var_faster.R")
source("count_alarms_hetero_faster.R")

# 10 knots using fractiles
load_min = 17
ii = load_train[[1]] >= load_min
knot_total = unique(quantile(load_train[[1]][ii],(0:(10 + 1))/(10 + 1),na.rm = TRUE))
knots = knot_total[2:(length(knot_total) - 1)] # remove endpoints

par(mfrow = c(1,1))
WT1_count_constant = count_alarms_constant_var_faster(load_train[[1]], log(vib_train[[1]]), knots,
                                              load_test[[1]], log(vib_test[[1]]), load_min = 17)
WT1_count_hetero = count_alarms_hetero_faster(load_train[[1]], log(vib_train[[1]]), knots,
                                              load_test[[1]], log(vib_test[[1]]), load_min = 17, 
                                              show_figure = TRUE, show_summary = FALSE)


################################################################################
## Simplify using loops
################################################################################
WT_res = NULL
for (i in 1:4){
  WT_temp = count_alarms_hetero_faster(load_train[[i]], log(vib_train[[i]]), knots,
                                           load_test[[i]], log(vib_test[[i]]), load_min = 17, 
                                           show_figure = FALSE, show_summary = FALSE)
  WT_res[[i]] = WT_temp$residuals
}

par(mfrow = c(2,2))
for (i in 1:4) {
  hist(WT_res[[i]], breaks = 200, main = i, xlim = c(-0.7,0.7))
}

length(WT_res[[2]])
length(WT2_count_hetero$residuals)
length(time_test[load_test[1,] >= load_min])

change = -0.2
load_min = 17 

par(mfrow = c(2,2))
change = -0.2
for (i in 1:4){
  plot(time_test[load_test[[i]] >= load_min][WT_res[[i]]>change], WT_res[[i]][WT_res[[i]]>change], 
       pch = ".", ylim = c(-0.4,0.4), xlab = "time", ylab = "residuals", main = i)
  points(time_test[load_test[[i]] >= load_min][WT_res[[i]]<change], WT_res[[i]][WT_res[[i]]<change], 
         pch = ".", col = "red")
}

## Use the same model on failure 

WT_res = NULL
for (i in 1:4){
  WT_temp = count_alarms_hetero_faster(load_train[[i]], log(vib_train[[i]]), knots,
                                       load_failure[[i]], log(vib_failure[[i]]), load_min = 17, 
                                       show_figure = FALSE, show_summary = FALSE)
  WT_res[[i]] = WT_temp$residuals
}

for (i in 1:4) {
  hist(WT_res[[i]], breaks = 200, main = i, xlim = c(-0.7,0.7))
}

change = -0.2
load_min = 17 

par(mfrow = c(2,2))
change = -0.2
for (i in 1:4){
  plot(time_failure[load_failure[[i]] >= load_min][WT_res[[i]]>change], WT_res[[i]][WT_res[[i]]>change], 
       pch = ".", ylim = c(-0.7,0.7), xlab = "time", ylab = "residuals", main = i)
  points(time_failure[load_failure[[i]] >= load_min][WT_res[[i]]<change], WT_res[[i]][WT_res[[i]]<change], 
         pch = ".", col = "red")
}
################################################################################
## Residual process
################################################################################
res = WT1_count_hetero$residuals
par(mfrow = c(1,1))
plot.ts(res)
res[is.na(res)][1:30]
par(mfrow = c(2,1), mar = rep(4,4))
acf(res[1:1900])
pacf(res[1:1900])
lag = 100 # 2000
par(mfrow = c(2,1), mar = c(4,4,3,1))
acf(res, na.action = na.pass, lag = lag) # starts at lag 0 !!
pacf(res, na.action = na.pass, lag = lag) # starts at lag 1 !
## Suggest it is an autoregressive process, since the PAF cuts of and the acf tails off slowly
# Maybe of order 10 AR(10). The acf decreases very slowly 

## Fit of AR(3)
ar3_fit = arima(res, order = c(3,0,0))
ar3_fit$aic # -43279.18

## Fit of AR(10)
ar10_fit = arima(res, order = c(10,0,0))
ar10_fit$aic # - 45018.82

auto.arima....

aic = matrix(NA,6,6)
for(p in 0:5) {
  for(q in 0:5) {
    arma_fit = arima(res, order = c(p,0,q))
    aic_temp = arma_fit$aic
    aic[p+1,q+1] = aic_temp
  }
}

aic
aic_extra = aic
aic_extra
which.min(aic)
min(aic)
aic[20]
data_aic = data.frame(aic)

library(knitr)
kable(head(load_train), digits=2)
kable(data_aic, digits = 2)
