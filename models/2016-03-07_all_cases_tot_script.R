#=========================================================================================
# Description: "first" monitor model
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
  
  # Interpolate -----------------------------------------------------------------------
  vib = names(bl)[2]; print(vib)
  load = names(bl)[3]; print(load)
  
  train = interpolate_func_2mins(bl, var = vib)
  time_train = train[[1]]
  vib_train = train[[2]]
  load_train = interpolate_func_2mins(bl, var = load)[[2]]
  
  test = interpolate_func_2mins(m, var = vib)
  time_test = test[[1]] 
  vib_test = test[[2]]
  load_test = interpolate_func_2mins(m, var = load)[[2]]

  # Training --------------------------------------------------------------------------
  no = 10
  if (names(bl[3]) == "GeneratorSpeed" ){
    load_min = 17
  } else if (names(bl[3]) == "GeR120RRefSpeedNoFilActual") {
    load_min = 11
  } else {
    load_min = 0
  }
  load_train_NA = replace(load_train, which(load_train < load_min), NA)
  log_vib_train = log(vib_train)
  log_vib_train_NA = replace(log_vib_train, which(is.infinite(log_vib_train)), NA)
  
  knot_total = unique(quantile(load_train_NA,(0:(no + 1))/(no + 1),na.rm = TRUE))
  knots = knot_total[2:(length(knot_total) - 1)] # remove endpoints
  spline_fit = lm(log_vib_train_NA ~ ns(load_train_NA, knots = knots), na.action = na.exclude)
  
  
  res = residuals(spline_fit) # also includes NAs so same lengt as original data
  
  ## Arma fit of residuals
  p = 2
  q = 2
  arma_fit = arima(res, order = c(p,0,q), include.mean = FALSE, optim.control = list(maxit = 400))
  arma_model = arma_fit$model
  sigma2_arma = arma_fit$sigma2
  
  # Testing ---------------------------------------------------------------------------
  load_test_NA = replace(load_test, which(load_test < load_min), NA)
  
  spline_predict = predict(spline_fit, data.frame(load_train_NA = load_test_NA))
  log_vib = log(vib_test)
  log_vib[is.infinite(log_vib)] = NA # remove inf !!
  res_test_spline = log_vib - spline_predict # order changed so gives positives
  
  ## Kalman and Alarm
  F =  t(matrix(arma_model$Z))
  G = arma_model$T
  Q = sigma2_arma * arma_model$Pn # sigma2 som er varians for white noise, Pn == H %*% t(H)
  
  # current estimates of state from training, mean and variance
  m0 = arma_model$a
  C0 = arma_model$P
  
  kalman = kalman_filter_arma(ts = res_test_spline, F = F, G = G, Q = Q, m0 = m0, C0 = C0)
  
  df_loop_sd = rbind(df_loop_sd, data.frame(time = time_test, sd = kalman$sd, index = i))
}
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
save(df_loop_sd, file = "df_loop_sd_178.RData")
