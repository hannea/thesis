#=========================================================================================
# Description: evaluation using "CUSUM" including alarm function and ROC plot
#
# Load: Output from models on all cases
# Save: ROC plot
#=========================================================================================
library(dplyr)
library(ggplot2)

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_cusum_added_LU.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_cusum_added_LU.RData")

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_cusum_added.RData")

# alarm ----------------------------------------------------------------------------------
alarm_df =
  df_loop_sd %>% 
  group_by(index) %>% 
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
            
            ## 2 in a row above ?sd
            date_2x3sd = time[min(which((abs_sd[-1] > 3 & abs_sd[-length(abs_sd)] > 3)))],
            date_2x4sd = time[min(which((abs_sd[-1] > 4 & abs_sd[-length(abs_sd)] > 4)))],
            date_2x5sd = time[min(which((abs_sd[-1] > 5 & abs_sd[-length(abs_sd)] > 5)))],
            date_2x6sd = time[min(which((abs_sd[-1] > 6 & abs_sd[-length(abs_sd)] > 6)))],
            date_2x7sd = time[min(which((abs_sd[-1] > 7 & abs_sd[-length(abs_sd)] > 7)))],
            date_2x8sd = time[min(which((abs_sd[-1] > 8 & abs_sd[-length(abs_sd)] > 8)))],
            
            ## 3 in a row above ?sd
            date_3x2 = time[min(which((abs_sd[-(1:2)] > 2 & abs_sd[-c(1,length(abs_sd))] > 2 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 2)))], 
            date_3x3 = time[min(which((abs_sd[-(1:2)] > 3 & abs_sd[-c(1,length(abs_sd))] > 3 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 3)))],            
            date_3x4 = time[min(which((abs_sd[-(1:2)] > 4 & abs_sd[-c(1,length(abs_sd))] > 4 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 4)))],
            date_3x5 = time[min(which((abs_sd[-(1:2)] > 5 & abs_sd[-c(1,length(abs_sd))] > 5 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 5)))],
            
            date_cusum_U1_3 = time[min(which(cusum_U1 > 3))],
            date_cusum_U1_4 = time[min(which(cusum_U1 > 4))],
            date_cusum_U1_5 = time[min(which(cusum_U1 > 5))],
            date_cusum_U1_6 = time[min(which(cusum_U1 > 6))],
            date_cusum_U1_7 = time[min(which(cusum_U1 > 7))],
            date_cusum_U1_8 = time[min(which(cusum_U1 > 8))],
            date_cusum_U1_9 = time[min(which(cusum_U1 > 9))],
            date_cusum_U1_10 = time[min(which(cusum_U1 > 10))],
            
            date_cusum_U2_3 = time[min(which(cusum_U2 > 3))],
            date_cusum_U2_4 = time[min(which(cusum_U2 > 4))],
            date_cusum_U2_5 = time[min(which(cusum_U2 > 5))],
            date_cusum_U2_6 = time[min(which(cusum_U2 > 6))],
            date_cusum_U2_7 = time[min(which(cusum_U2 > 7))],
            date_cusum_U2_8 = time[min(which(cusum_U2 > 8))],
            date_cusum_U2_9 = time[min(which(cusum_U2 > 9))],
            date_cusum_U2_10 = time[min(which(cusum_U2 > 10))],
            
            date_cusum_U3_3 = time[min(which(cusum_U3 > 3))],
            date_cusum_U3_4 = time[min(which(cusum_U3 > 4))],
            date_cusum_U3_5 = time[min(which(cusum_U3 > 5))],
            date_cusum_U3_6 = time[min(which(cusum_U3 > 6))],
            date_cusum_U3_7 = time[min(which(cusum_U3 > 7))],
            date_cusum_U3_8 = time[min(which(cusum_U3 > 8))],
            date_cusum_U3_9 = time[min(which(cusum_U3 > 9))],
            date_cusum_U3_10 = time[min(which(cusum_U3 > 10))],
            
            date_cusum_U4_3 = time[min(which(cusum_U4 > 3))],
            date_cusum_U4_4 = time[min(which(cusum_U4 > 4))],
            date_cusum_U4_5 = time[min(which(cusum_U4 > 5))],
            date_cusum_U4_6 = time[min(which(cusum_U4 > 6))],
            date_cusum_U4_7 = time[min(which(cusum_U4 > 7))],
            date_cusum_U4_8 = time[min(which(cusum_U4 > 8))],
            date_cusum_U4_9 = time[min(which(cusum_U4 > 9))],
            date_cusum_U4_10 = time[min(which(cusum_U4 > 10))],
            
            date_cusum_U5_3 = time[min(which(cusum_U5 > 3))],
            date_cusum_U5_4 = time[min(which(cusum_U5 > 4))],
            date_cusum_U5_5 = time[min(which(cusum_U5 > 5))],
            date_cusum_U5_6 = time[min(which(cusum_U5 > 6))],
            date_cusum_U5_7 = time[min(which(cusum_U5 > 7))],
            date_cusum_U5_8 = time[min(which(cusum_U5 > 8))],
            date_cusum_U5_9 = time[min(which(cusum_U5 > 9))],
            date_cusum_U5_10 = time[min(which(cusum_U5 > 10))],
            
            date_cusum_L1_3 = time[min(which(cusum_L1 > 3))],
            date_cusum_L1_4 = time[min(which(cusum_L1 > 4))],
            date_cusum_L1_5 = time[min(which(cusum_L1 > 5))],
            date_cusum_L1_6 = time[min(which(cusum_L1 > 6))],
            date_cusum_L1_7 = time[min(which(cusum_L1 > 7))],
            date_cusum_L1_8 = time[min(which(cusum_L1 > 8))],
            date_cusum_L1_9 = time[min(which(cusum_L1 > 9))],
            date_cusum_L1_10 = time[min(which(cusum_L1 > 10))],
            
            date_cusum_L2_3 = time[min(which(cusum_L2 > 3))],
            date_cusum_L2_4 = time[min(which(cusum_L2 > 4))],
            date_cusum_L2_5 = time[min(which(cusum_L2 > 5))],
            date_cusum_L2_6 = time[min(which(cusum_L2 > 6))],
            date_cusum_L2_7 = time[min(which(cusum_L2 > 7))],
            date_cusum_L2_8 = time[min(which(cusum_L2 > 8))],
            date_cusum_L2_9 = time[min(which(cusum_L2 > 9))],
            date_cusum_L2_10 = time[min(which(cusum_L2 > 10))],
            
            date_cusum_L3_3 = time[min(which(cusum_L3 > 3))],
            date_cusum_L3_4 = time[min(which(cusum_L3 > 4))],
            date_cusum_L3_5 = time[min(which(cusum_L3 > 5))],
            date_cusum_L3_6 = time[min(which(cusum_L3 > 6))],
            date_cusum_L3_7 = time[min(which(cusum_L3 > 7))],
            date_cusum_L3_8 = time[min(which(cusum_L3 > 8))],
            date_cusum_L3_9 = time[min(which(cusum_L3 > 9))],
            date_cusum_L3_10 = time[min(which(cusum_L3 > 10))],
            
            date_cusum_L4_3 = time[min(which(cusum_L4 > 3))],
            date_cusum_L4_4 = time[min(which(cusum_L4 > 4))],
            date_cusum_L4_5 = time[min(which(cusum_L4 > 5))],
            date_cusum_L4_6 = time[min(which(cusum_L4 > 6))],
            date_cusum_L4_7 = time[min(which(cusum_L4 > 7))],
            date_cusum_L4_8 = time[min(which(cusum_L4 > 8))],
            date_cusum_L4_9 = time[min(which(cusum_L4 > 9))],
            date_cusum_L4_10 = time[min(which(cusum_L4 > 10))],
            
            date_cusum_L5_3 = time[min(which(cusum_L5 > 3))],
            date_cusum_L5_4 = time[min(which(cusum_L5 > 4))],
            date_cusum_L5_5 = time[min(which(cusum_L5 > 5))],
            date_cusum_L5_6 = time[min(which(cusum_L5 > 6))],
            date_cusum_L5_7 = time[min(which(cusum_L5 > 7))],
            date_cusum_L5_8 = time[min(which(cusum_L5 > 8))],
            date_cusum_L5_9 = time[min(which(cusum_L5 > 9))],
            date_cusum_L5_10 = time[min(which(cusum_L5 > 10))]
  )

load("~/aspeciale/R_data_local/all_cases_RData/meta_tot_df.RData")
obs = as.POSIXct(meta_df$knownalarm, tz = "GMT")


# ggplot ----------------------------------------------------------------------------
ROC = NULL

for(i in 3:length(alarm_df)){
  pred_time = alarm_df[[i]]
  dt = difftime(pred_time, obs, units = "days")
  scored = sapply(dt, function(x) 1/(1 + exp((0.22*x-0))))
  TPR = sum(scored, na.rm = TRUE)/ sum(!is.na(obs[1:length(pred_time)]))
  FPR = sum((!is.na(pred_time)) > (!is.na(obs[1:length(pred_time)])))/
    sum(is.na(obs[1:length(pred_time)])) # FP/N ,  N = FP + TN
  
  if (grepl("_U", names(alarm_df[i]))) {
    alarm = "upper"
  } else if (grepl("_L", names(alarm_df[i]))) {
    alarm = "lower"
  } else {alarm = "simple"}
  ROC = rbind(ROC, data.frame(FPR, TPR, above = names(alarm_df[i]), alarm = alarm))
}


ggplot(ROC, aes(FPR, TPR, col = alarm)) + geom_point() +  # , col = name in aes()
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)

# save df ROC -----------------------------------------------------------------------

#ROC$method = "interp_later"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_interp_later_cusum.RData")

#ROC$method = "only_spline"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_only_spline_cusum.RData")


# Several methods -------------------------------------------------------------------
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_cusum.RData")
ROC_methods = ROC
load("ROC_score_only_spline_cusum.RData")
ROC_methods = rbind(ROC_methods, ROC)


ROC_methods$alarm = factor(ROC_methods$alarm, levels = unique(ROC_methods$alarm)) #otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = alarm)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)+ # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method) +
  scale_color_discrete(labels = c("simple", "cusum_U", "cusum_L"))

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_interp_later_only_spline_cusum.png", width = 7, height = 3.4)



