#=========================================================================================
# Description: Joins the output of the monitor model to new columns calculated using
# rolling sums
#
# Load: output from monitor models
# Save: joined dataframe with extra columns
#=========================================================================================
library(dplyr)
library(ggplot2)
library(zoo)

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res.RData")     # abs_res
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30.RData") # interp_later
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline.RData")             # only_spline
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_30.RData")          # only_spline_30

# load("~/documents/saved/df_loop_sd_178_interp_later_abs_res_30.RData")

## final 
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non_10.RData") #interp_later (10)
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non.RData") #only_spline (abs) (10)

# zoo_package -----------------------------------------------------------------------

## larger than 2,3,4 sd
for (i in 2:4) {
  logic = abs(df_loop_sd$sd) > i
  logic[is.na(logic)] = FALSE
  sum_logic = rollapply(logic, 30, sum, fill = NA, align = "r", partial = TRUE)
  df_loop_sd$added = sum_logic
  names(df_loop_sd)[length(df_loop_sd)] = paste0("rollsum", i, "sd") # remove added name and replace with cusumk
}

## above 3 (3:5) sd (observed bigger than predicted)
for (i in 3:3) {
  logic_plus = df_loop_sd$sd > i
  logic_plus[is.na(logic_plus)] = FALSE
  sum_logic = rollapply(logic_plus, 30, sum, fill = NA, align = "r", partial = TRUE) 
  df_loop_sd$added = sum_logic
  names(df_loop_sd)[length(df_loop_sd)] = paste0("rollsum", i, "sd_plus") # remove added name and replace with cusumk
}

## below -3 (-(3:5)) sd (observed smaller than predicted)
for (i in 3:3) {
logic_minus = df_loop_sd$sd < -i
logic_minus[is.na(logic_minus)] = FALSE
sum_logic = rollapply(logic_minus, 30, sum, fill = NA, align = "r", partial = TRUE) 
df_loop_sd$added = sum_logic
names(df_loop_sd)[length(df_loop_sd)] = paste0("rollsum", i, "sd_minus") # remove added name and replace with cusumk
}

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#setwd("~/documents/saved")
#save(df_loop_sd, file = "df_loop_sd_178_interp_later_abs_res_rollsums_added.RData")
#save(df_loop_sd, file = "df_loop_sd_178_interp_later_abs_res_30_rollsums_added.RData")
#save(df_loop_sd, file = "df_loop_sd_178_only_spline_rollsums_added.RData")
#save(df_loop_sd, file = "df_loop_sd_178_only_spline_30_rollsums_added.RData")


#save(df_loop_sd, file = "df_loop_sd_178_interp_later_abs_res_non_rollsums_added.RData")
#save(df_loop_sd, file = "df_loop_sd_178_only_spline_abs_res_non_rollsums_added.RData")