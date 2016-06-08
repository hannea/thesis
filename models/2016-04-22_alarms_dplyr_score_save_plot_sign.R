#=========================================================================================
# Description: evaluation using "sign" including alarm function and ROC plot
#
# Load: Output from models on all cases
# Save: ROC plot
#=========================================================================================
library(dplyr)
library(ggplot2)


load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res.RData") # interp_later
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30.RData")
## can not be handled as the other
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_learn_max.RData") # learn_max

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline.RData") # only_spline
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_30.RData")


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
            
            date_3sd_plus = time[min(which(sd > 3))],
            date_4sd_plus = time[min(which(sd > 4))],
            date_5sd_plus = time[min(which(sd > 5))],
            date_6sd_plus = time[min(which(sd > 6))],
            date_7sd_plus = time[min(which(sd > 7))],
            date_8sd_plus = time[min(which(sd > 8))],
            date_9sd_plus = time[min(which(sd > 9))],
            date_10sd_plus = time[min(which(sd > 10))],
            
            date_3sd_minus = time[min(which(sd < -3))],
            date_4sd_minus = time[min(which(sd < -4))],
            date_5sd_minus = time[min(which(sd < -5))],
            date_6sd_minus = time[min(which(sd < -6))],
            date_7sd_minus = time[min(which(sd < -7))],
            date_8sd_minus = time[min(which(sd < -8))],
            date_9sd_minus = time[min(which(sd < -9))],
            date_10sd_minus = time[min(which(sd < -10))],
            
            ## 2 in a row above ?sd
            date_2x3sd = time[min(which((abs_sd[-1] > 3 & abs_sd[-length(abs_sd)] > 3)))],
            date_2x4sd = time[min(which((abs_sd[-1] > 4 & abs_sd[-length(abs_sd)] > 4)))],
            date_2x5sd = time[min(which((abs_sd[-1] > 5 & abs_sd[-length(abs_sd)] > 5)))],
            date_2x6sd = time[min(which((abs_sd[-1] > 6 & abs_sd[-length(abs_sd)] > 6)))],
            date_2x7sd = time[min(which((abs_sd[-1] > 7 & abs_sd[-length(abs_sd)] > 7)))],
            date_2x8sd = time[min(which((abs_sd[-1] > 8 & abs_sd[-length(abs_sd)] > 8)))],
            
            date_2x3sd_plus = time[min(which((sd[-1] > 3 & sd[-length(sd)] > 3)))],
            date_2x4sd_plus = time[min(which((sd[-1] > 4 & sd[-length(sd)] > 4)))],
            date_2x5sd_plus = time[min(which((sd[-1] > 5 & sd[-length(sd)] > 5)))],
            date_2x6sd_plus = time[min(which((sd[-1] > 6 & sd[-length(sd)] > 6)))],
            date_2x7sd_plus = time[min(which((sd[-1] > 7 & sd[-length(sd)] > 7)))],
            date_2x8sd_plus = time[min(which((sd[-1] > 8 & sd[-length(sd)] > 8)))],
            
            date_2x3sd_minus = time[min(which((sd[-1] < -3 & sd[-length(sd)] < -3)))],
            date_2x4sd_minus = time[min(which((sd[-1] < -4 & sd[-length(sd)] < -4)))],
            date_2x5sd_minus = time[min(which((sd[-1] < -5 & sd[-length(sd)] < -5)))],
            date_2x6sd_minus = time[min(which((sd[-1] < -6 & sd[-length(sd)] < -6)))],
            date_2x7sd_minus = time[min(which((sd[-1] < -7 & sd[-length(sd)] < -7)))],
            date_2x8sd_minus = time[min(which((sd[-1] < -8 & sd[-length(sd)] < -8)))],
            
            ## 3 in a row above ?sd
            date_3x2 = time[min(which((abs_sd[-(1:2)] > 2 & abs_sd[-c(1,length(abs_sd))] > 2 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 2)))], 
            date_3x3 = time[min(which((abs_sd[-(1:2)] > 3 & abs_sd[-c(1,length(abs_sd))] > 3 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 3)))],            
            date_3x4 = time[min(which((abs_sd[-(1:2)] > 4 & abs_sd[-c(1,length(abs_sd))] > 4 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 4)))],
            date_3x5 = time[min(which((abs_sd[-(1:2)] > 5 & abs_sd[-c(1,length(abs_sd))] > 5 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 5)))],
            
            date_3x2_plus = time[min(which((sd[-(1:2)] > 2 & sd[-c(1,length(sd))] > 2 & 
                                         sd[-c((length(sd) - 1),length(sd))] > 2)))], 
            date_3x3_plus = time[min(which((sd[-(1:2)] > 3 & sd[-c(1,length(sd))] > 3 & 
                                         sd[-c((length(sd) - 1),length(sd))] > 3)))],            
            date_3x4_plus = time[min(which((sd[-(1:2)] > 4 & sd[-c(1,length(sd))] > 4 & 
                                         sd[-c((length(sd) - 1),length(sd))] > 4)))],
            date_3x5_plus = time[min(which((sd[-(1:2)] > 5 & sd[-c(1,length(sd))] > 5 & 
                                         sd[-c((length(sd) - 1),length(sd))] > 5)))],
            
            date_3x2_minus = time[min(which((sd[-(1:2)] < -2 & sd[-c(1,length(sd))] < -2 & 
                                              sd[-c((length(sd) - 1),length(sd))] < -2)))], 
            date_3x3_minus = time[min(which((sd[-(1:2)] < -3 & sd[-c(1,length(sd))] < -3 & 
                                              sd[-c((length(sd) - 1),length(sd))] < -3)))],            
            date_3x4_minus = time[min(which((sd[-(1:2)] < -4 & sd[-c(1,length(sd))] < -4 & 
                                              sd[-c((length(sd) - 1),length(sd))] < -4)))],
            date_3x5_minus = time[min(which((sd[-(1:2)] < -5 & sd[-c(1,length(sd))] < -5 & 
                                              sd[-c((length(sd) - 1),length(sd))] < -5)))]
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
  
  if (grepl("plus", names(alarm_df[i]))) {
    alarm = "plus"
  } else if (grepl("minus", names(alarm_df[i]))) {
    alarm = "minus"
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
#save(ROC, file = "ROC_score_interp_later_sign.RData")

#ROC$method = "only_spline"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_only_spline_sign.RData")


# Several methods -------------------------------------------------------------------
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_sign.RData")
ROC_methods = ROC
load("ROC_score_only_spline_sign.RData")
ROC_methods = rbind(ROC_methods, ROC)


ROC_methods$alarm = factor(ROC_methods$alarm, levels = unique(ROC_methods$alarm)) #otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = alarm)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)+ # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method) +
  scale_color_discrete(name = "sign")
#, breaks=c("simple", "plus", "minus"), labels = c("both", "plus", "minus"))

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_interp_later_only_spline_sign.png", width = 7, height = 3.4)



