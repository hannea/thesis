#=========================================================================================
# Description: "subset_by_power" monitor model
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

for (i in 0:178) {
  print(i)
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  load(bl_file_name)
  m_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_m.RData', sep = "")
  load(m_file_name)

  # Identify --------------------------------------------------------------------------
  bl = tbl_df(bl)
  bl[[1]] = as.POSIXct(bl[[1]], tz = "GMT")
  bl[[2]] = replace(bl[[2]], which(bl$PowerActual < 100), NA)
  bl[[3]] = replace(bl[[3]], which(bl$PowerActual < 100), NA)
  
  m = tbl_df(m)
  m[[1]] = as.POSIXct(m[[1]], tz = "GMT")
  m[[2]] = replace(m[[2]], which(m$PowerActual < 100), NA)
  m[[3]] = replace(m[[3]], which(m$PowerActual < 100), NA)
  
  if (sum(bl$PowerActual >= 100) == 0) {
    warning("Power never above 100 kW in training data in dataset number ", i, ".")
    next
  }

  # Training --------------------------------------------------------------------------
  no = 30

  load_train_NA = bl[[3]]
  log_vib_train = (bl[[2]])
  log_vib_train[is.infinite(log_vib_train)] = NA # remove inf !!
  
  knot_total = unique(quantile(load_train_NA,(0:(no + 1))/(no + 1),na.rm = TRUE))
  knots = knot_total[2:(length(knot_total) - 1)] # remove endpoints
  spline_fit = lm(log_vib_train ~ ns(load_train_NA, knots = knots), na.action = na.exclude)
  
  res = residuals(spline_fit) #also includes NAs so same lengt as original data instead of spline_fit$residuals

  abs_res_spline_fit = lm(abs(res) ~ ns(load_train_NA, knots = knots), na.action = na.exclude)
  abs_res_predict = predict(abs_res_spline_fit, data.frame(load_train_NA = load_train_NA))
  
  #plot(load_train_NA, abs(res), pch = ".")
  #hist(res, breaks = 100)
  #points(load_train_NA, abs_res_predict, pch = 20, col = "red")
  #abline(v = knots, h = 0)
  
  res_train_spline = res/abs_res_predict
  
  #plot(load_train_NA, res_train_spline, pch = ".")
  
  # Interpolate train -----------------------------------------------------------------
  res_train_df = data.frame(bl[1],res_train_spline)
  train = interpolate_func_2mins(res_train_df, var = "res_train_spline")
  time_train = train[[1]]
  res_train = train[[2]]
  
  ## Arma fit of residuals
  p = 2
  q = 2
  arma_fit = arima(res_train, order = c(p,0,q), include.mean = FALSE, optim.control = list(maxit = 1000))
  arma_model = arma_fit$model
  sigma2_arma = arma_fit$sigma2
  
  # Testing ---------------------------------------------------------------------------
  load_test_NA = m[[3]]
  
  spline_predict = predict(spline_fit, data.frame(load_train_NA = load_test_NA))
  log_vib_test = (m[[2]])
  log_vib_test[is.infinite(log_vib_test)] = NA # remove inf !!
  
  abs_res_predict_test = predict(abs_res_spline_fit, data.frame(load_train_NA = load_test_NA))
  
  res_test_spline = (log_vib_test - spline_predict)/abs_res_predict_test
  
  # Interpolate test ------------------------------------------------------------------
  res_test_df = data.frame(m[1],res_test_spline)
  #ggplot(res_test_df, aes(CMSTimeStamp, res_test_spline)) + geom_point()
  test = interpolate_func_2mins(res_test_df, var = "res_test_spline")
  time_test = test[[1]] 
  res_test_interp = test[[2]]
  #qplot(time_test, res_test_interp)

  ## Kalman and Alarm
  F =  t(matrix(arma_model$Z))
  G = arma_model$T
  Q = sigma2_arma * arma_model$Pn # sigma2 som er varians for white noise, Pn == H %*% t(H)
  
  # current estimates of state from training, mean and variance
  m0 = arma_model$a
  C0 = arma_model$P
  
  kalman = kalman_filter_arma(ts = res_test_interp, F = F, G = G, Q = Q, m0 = m0, C0 = C0)
  qplot(time_test, kalman$sd)
  df_loop_sd = rbind(df_loop_sd, data.frame(time = time_test, sd = kalman$sd, index = i))
}
#setwd("~/aspeciale/R_data_local")
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
save(df_loop_sd, file = "df_loop_sd_178_interp_later_abs_res_non_30_subset_by_power.RData")

