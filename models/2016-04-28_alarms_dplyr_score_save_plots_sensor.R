#=========================================================================================
# Description: evaluation script using the sensor location in alarm function and ROC plot
#
# Load: Output from joined sensor location and model on all cases
# Save: ROC plot
#=========================================================================================
library(dplyr)
library(ggplot2)

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178.RData") # first
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_abs_res.RData") # abs_res
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res.RData") # interp_later
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30.RData")
## can not be handled as the other
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_learn_max.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_30.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30_subset_by_power.RData")

## final 
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non_10.RData") #interp_later (10)
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non.RData") #only_spline (abs) (10)


# Divide into sensors ---------------------------------------------------------------

load("~/aspeciale/R_data_local/all_cases_RData/sensor_names.RData")
df_loop_sd_sensor = inner_join(df_loop_sd, sensor_df)

alarm_df =
  df_loop_sd_sensor %>% 
  group_by(index, sensor) %>% 
  filter(1:length(sd) > 50 & 1:length(sd) < (length(sd)-50) , !is.na(sd)) %>% 
  mutate(abs_sd = abs(sd)) %>% 
  summarise(n = n(), 
            
            ## above ?sd
            date_3sd = time[min(which(abs_sd > 3))],
            date_4sd = time[min(which(abs_sd > 4))],
            date_5sd = time[min(which(abs_sd > 5))],
            date_6sd = time[min(which(abs_sd > 6))],
            date_7sd = time[min(which(abs_sd > 7))],
            date_8sd = time[min(which(abs_sd > 8))],
            date_9sd = time[min(which(abs_sd > 9))],
            date_10sd = time[min(which(abs_sd > 10))],
            date_11sd = time[min(which(abs_sd > 11))],
            
            ## 2 in a row above ?sd
            date_2x3sd = time[min(which((abs_sd[-1] > 3 & abs_sd[-length(abs_sd)] > 3)))],
            date_2x4sd = time[min(which((abs_sd[-1] > 4 & abs_sd[-length(abs_sd)] > 4)))],
            date_2x5sd = time[min(which((abs_sd[-1] > 5 & abs_sd[-length(abs_sd)] > 5)))],
            date_2x6sd = time[min(which((abs_sd[-1] > 6 & abs_sd[-length(abs_sd)] > 6)))],
            date_2x7sd = time[min(which((abs_sd[-1] > 7 & abs_sd[-length(abs_sd)] > 7)))],
            date_2x8sd = time[min(which((abs_sd[-1] > 8 & abs_sd[-length(abs_sd)] > 8)))],
            date_2x9sd = time[min(which((abs_sd[-1] > 9 & abs_sd[-length(abs_sd)] > 9)))],
            date_2x10sd = time[min(which((abs_sd[-1] > 10 & abs_sd[-length(abs_sd)] > 10)))],
            
            ## 3 in a row above ?sd
            date_3x2 = time[min(which((abs_sd[-(1:2)] > 2 & abs_sd[-c(1,length(abs_sd))] > 2 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 2)))], 
            date_3x3 = time[min(which((abs_sd[-(1:2)] > 3 & abs_sd[-c(1,length(abs_sd))] > 3 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 3)))],            
            date_3x4 = time[min(which((abs_sd[-(1:2)] > 4 & abs_sd[-c(1,length(abs_sd))] > 4 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 4)))],
            date_3x5 = time[min(which((abs_sd[-(1:2)] > 5 & abs_sd[-c(1,length(abs_sd))] > 5 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 5)))]
  )
#setwd("~/aspeciale")
#save(alarm_df, file = "alarm_df_interp_later_abs_res_30.RData")

load("~/aspeciale/R_data_local/all_cases_RData/meta_tot_df.RData")
obs = as.POSIXct(meta_df$knownalarm, tz = "GMT")

#load("~/aspeciale/R_data_local/all_cases_RData/meta_u174_df.RData")
#obs = as.POSIXct(meta_u174$knownalarm, tz = "GMT")
obs_df = data.frame(obs, sensor_df)
obs_generator = filter(obs_df, sensor == "generator")[[1]]
obs_gearbox = filter(obs_df, sensor == "gearbox")[[1]]
obs_main_bearing = filter(obs_df, sensor == "main_bearing")[[1]]

# ggplot ----------------------------------------------------------------------------

alarm_df_generator = filter(alarm_df, sensor == "generator")
alarm_df_gearbox = filter(alarm_df, sensor == "gearbox")
alarm_df_main_bearing = filter(alarm_df, sensor == "main_bearing")

ROC_sensor = NULL
ROC = NULL

for(i in 4:length(alarm_df_generator)){
  pred_time = alarm_df_generator[[i]]
  dt = difftime(pred_time, obs_generator, units = "days")
  scored = sapply(dt, function(x) 1/(1 + exp((0.22*x-0)))) # score all the. minus the 50 % center, eg. 10 days
  TPR = sum(scored, na.rm = TRUE)/ sum(!is.na(obs_generator[1:length(pred_time)]))
  FPR = sum((!is.na(pred_time)) > (!is.na(obs_generator[1:length(pred_time)])))/
    sum(is.na(obs_generator[1:length(pred_time)])) # FP/N ,  N = FP + TN
  ROC = rbind(ROC, data.frame(FPR, TPR, above = names(alarm_df[i]), sensor = "generator"))
}

ROC_sensor = ROC
# gearbox ---------------------------------------------------------------------------
ROC = NULL

for(i in 4:length(alarm_df_gearbox)){
  pred_time = alarm_df_gearbox[[i]]
  dt = difftime(pred_time, obs_gearbox, units = "days")
  scored = sapply(dt, function(x) 1/(1 + exp((0.22*x-0)))) # score all the. minus the 50 % center, eg. 10 days
  TPR = sum(scored, na.rm = TRUE)/ sum(!is.na(obs_gearbox[1:length(pred_time)]))
  FPR = sum((!is.na(pred_time)) > (!is.na(obs_gearbox[1:length(pred_time)])))/
    sum(is.na(obs_gearbox[1:length(pred_time)])) # FP/N ,  N = FP + TN
  ROC = rbind(ROC, data.frame(FPR, TPR, above = names(alarm_df[i]),sensor = "gearbox"))
}

ROC_sensor = rbind(ROC_sensor, ROC)

# main bearing ---------------------------------------------------------------------------
ROC = NULL
for(i in 4:length(alarm_df_main_bearing)){
  pred_time = alarm_df_main_bearing[[i]]
  dt = difftime(pred_time, obs_main_bearing, units = "days")
  scored = sapply(dt, function(x) 1/(1 + exp((0.22*x-0)))) # score all the. minus the 50 % center, eg. 10 days
  TPR = sum(scored, na.rm = TRUE)/ sum(!is.na(obs_main_bearing[1:length(pred_time)]))
  FPR = sum((!is.na(pred_time)) > (!is.na(obs_main_bearing[1:length(pred_time)])))/
    sum(is.na(obs_main_bearing[1:length(pred_time)])) # FP/N ,  N = FP + TN
  ROC = rbind(ROC, data.frame(FPR, TPR, above = names(alarm_df[i]), sensor = "main_bearing"))
}
ROC_sensor = rbind(ROC_sensor, ROC)

# ROC_sensor$method = "interp_later"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC_sensor, file = "ROC_score_interp_later_sensor.RData")

# ROC_sensor$method = "only_spline"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC_sensor, file = "ROC_score_only_spline_sensor.RData")

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_sensor.RData")
ROC_methods = ROC_sensor
load("ROC_score_only_spline_sensor.RData")
ROC_methods = rbind(ROC_methods, ROC_sensor)


ggplot(ROC_methods, aes(FPR, TPR, col = sensor)) + geom_point() +
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) +
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_sensors.png", width = 7, height = 3.4)
