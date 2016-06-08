#=========================================================================================
# Description: "only_spline" monitor model
#
# Load: all cases as RData files
# Save: a data frame containing interpolated times in test data, the residual process and
#       the case index
#=========================================================================================
library(dplyr)
library(ggplot2)
library(splines)
library(monitor)

df_loop_sd = NULL

for (i in 0:178){
  print(i)
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  load(bl_file_name)
  m_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_m.RData', sep = "")
  load(m_file_name)
  
  # Identify --------------------------------------------------------------------------
  bl = tbl_df(bl)
  bl[[1]] = as.POSIXct(bl[[1]], tz = "GMT")
  
  m = tbl_df(m)
  m[[1]] = as.POSIXct(m[[1]], tz = "GMT")
  
  # Training --------------------------------------------------------------------------
  no = 30
  print(names(bl[3]))
  if (names(bl[3]) == "GeneratorSpeed" ){
    load_min = 17
  } else if (names(bl[3]) == "GeR120RRefSpeedNoFilActual") {
   load_min = 11
  } else {
    load_min = 0
  }
  load_train_NA = replace(bl[[3]], which(bl[[3]] < load_min), NA)
  log_vib_train = log(bl[[2]])
  log_vib_train_NA = replace(log_vib_train, which(is.infinite(log_vib_train)), NA)
  #plot(load_train_NA, log_vib_train_NA, pch = ".")
  
  knot_total = unique(quantile(load_train_NA,(0:(no + 1))/(no + 1),na.rm = TRUE))
  knots = knot_total[2:(length(knot_total) - 1)] # remove endpoints
  spline_fit = lm(log_vib_train_NA ~ ns(load_train_NA, knots = knots), na.action = na.exclude)
  
  bins = 100
  quantile_load = unique(quantile(load_train_NA,(0:bins)/bins,na.rm=TRUE))
  sload = cut(load_train_NA, breaks = quantile_load) # factor
  sd_vib = tapply(log_vib_train_NA, sload, sd, na.rm = TRUE)
  mean_vib = tapply(log_vib_train_NA, sload, mean, na.rm = TRUE)
  quantile_mean = (quantile_load[-1]+quantile_load[1:length(quantile_load)-1])/2
  
  sd_spline_fit = lm(sd_vib ~ ns(quantile_mean, knots = knots), na.action = na.exclude)

  # Testing ---------------------------------------------------------------------------
  load_test_NA = replace(m[[3]], which(m[[3]] < load_min), NA)
  
  spline_predict = predict(spline_fit, data.frame(load_train_NA = load_test_NA))
  sd_predict = predict(sd_spline_fit, data.frame(quantile_mean = load_test_NA))
  
  log_vib_test = log(m[[2]])
  log_vib_test[is.infinite(log_vib_test)] = NA # remove inf !!
  
  ## Plot
  plot = FALSE
  if (plot == TRUE) {
    plot(load_test_NA, spline_predict, pch = 20)#, ylim = c(-1.5, 0))
    points(load_test_NA, spline_predict + sd_predict, col = "red", pch = 20)
    points(load_test_NA, spline_predict - sd_predict, col = "red", pch = 20)
    
    points(load_test_NA, log_vib_test, col = "blue", pch = ".")
    points(load_train_NA, log_vib_train_NA, col = "green", pch = ".")
    points(quantile_mean, mean_vib, col = "green", pch = 20)
    points(quantile_mean, mean_vib + sd_vib, col = "green", pch = 20)
    points(quantile_mean, mean_vib - sd_vib, col = "green", pch = 20)
  }

  ## standardized residual process
  st_res = (log_vib_test - spline_predict)/sd_predict
  
  if (plot == TRUE) {
    sd_predict_train = predict(sd_spline_fit, data.frame(quantile_mean = load_train_NA))
    plot(load_test_NA, st_res, pch = ".")
    points(load_train_NA, residuals(spline_fit)/sd_predict_train, pch = ".", col = "green") 
  }
  
  df_loop_sd = rbind(df_loop_sd, data.frame(time = m[[1]], sd = st_res, index = i))
}
#setwd("~/aspeciale/R_data_local")
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(df_loop_sd, file = "df_loop_sd_1_only_spline_30.RData") #_bins_changed_30_knots
