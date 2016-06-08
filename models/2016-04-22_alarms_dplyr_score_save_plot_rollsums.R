#=========================================================================================
# Description: evaluation using "rollsum" including alarm function and ROC plot
#
# Load: Output from models on all cases
# Save: ROC plot
#=========================================================================================
library(dplyr)
library(ggplot2)

# zoo_package -----------------------------------------------------------------------

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_rollsums_added.RData")
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30_rollsums_added.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_rollsums_added.RData")
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_30_rollsums_added.RData")

## final rollsum
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non_rollsums_added.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non_rollsums_added.RData")


## after rollsum added
alarm_df =
  df_loop_sd %>% 
  group_by(index) %>% 
  filter(1:length(sd) > 50 & 1:length(sd) < (length(sd)-50) , !is.na(sd)) %>% 
  mutate(abs_sd = abs(sd)) %>% 
  summarise(n = n(), 
            
            ## these are not as good as rollsum
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
            #date_2x9sd = time[min(which((abs_sd[-1] > 9 & abs_sd[-length(abs_sd)] > 9)))],
            #date_2x10sd = time[min(which((abs_sd[-1] > 10 & abs_sd[-length(abs_sd)] > 10)))],
            
            ## 3 in a row above ?sd
            date_3x3 = time[min(which((abs_sd[-(1:2)] > 3 & abs_sd[-c(1,length(abs_sd))] > 3 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 3)))],            
            date_3x4 = time[min(which((abs_sd[-(1:2)] > 4 & abs_sd[-c(1,length(abs_sd))] > 4 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 4)))],
            date_3x5 = time[min(which((abs_sd[-(1:2)] > 5 & abs_sd[-c(1,length(abs_sd))] > 5 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 5)))],
            
            date_rollsum2_3 = time[min(which(rollsum2sd > 3))],
            date_rollsum2_4 = time[min(which(rollsum2sd > 4))],
            date_rollsum2_5 = time[min(which(rollsum2sd > 5))],
            date_rollsum2_6 = time[min(which(rollsum2sd > 6))],
            date_rollsum2_7 = time[min(which(rollsum2sd > 7))],
            date_rollsum2_8 = time[min(which(rollsum2sd > 8))],
            date_rollsum2_9 = time[min(which(rollsum2sd > 9))],
            date_rollsum2_10 = time[min(which(rollsum2sd > 10))],
            date_rollsum2_11 = time[min(which(rollsum2sd > 11))],
            date_rollsum2_12 = time[min(which(rollsum2sd > 12))],
            date_rollsum2_13 = time[min(which(rollsum2sd > 13))],
            
            date_rollsum3 = time[min(which(rollsum3sd > 3))],
            date_rollsum4 = time[min(which(rollsum3sd > 4))],
            date_rollsum5 = time[min(which(rollsum3sd > 5))],
            date_rollsum6 = time[min(which(rollsum3sd > 6))],
            date_rollsum7 = time[min(which(rollsum3sd > 7))],
            date_rollsum8 = time[min(which(rollsum3sd > 8))],
            date_rollsum9 = time[min(which(rollsum3sd > 9))],
            date_rollsum10 = time[min(which(rollsum3sd > 10))],
            date_rollsum11 = time[min(which(rollsum3sd > 11))],
            date_rollsum12 = time[min(which(rollsum3sd > 12))],
            date_rollsum13 = time[min(which(rollsum3sd > 13))],
            date_rollsum14 = time[min(which(rollsum3sd > 14))],
            date_rollsum15 = time[min(which(rollsum3sd > 15))],
            
            date_rollsum4_3 = time[min(which(rollsum4sd > 3))],
            date_rollsum4_4 = time[min(which(rollsum4sd > 4))],
            date_rollsum4_5 = time[min(which(rollsum4sd > 5))],
            date_rollsum4_6 = time[min(which(rollsum4sd > 6))],
            date_rollsum4_7 = time[min(which(rollsum4sd > 7))],
            date_rollsum4_8 = time[min(which(rollsum4sd > 8))],
            date_rollsum4_9 = time[min(which(rollsum4sd > 9))],
            date_rollsum4_10 = time[min(which(rollsum4sd > 10))],
            date_rollsum4_11 = time[min(which(rollsum4sd > 11))],
            date_rollsum4_12 = time[min(which(rollsum4sd > 12))],
            date_rollsum4_13 = time[min(which(rollsum4sd > 13))],
            date_rollsum4_14 = time[min(which(rollsum4sd > 14))],
            date_rollsum4_15 = time[min(which(rollsum4sd > 15))],
            date_rollsum4_16 = time[min(which(rollsum4sd > 16))],
            date_rollsum4_17 = time[min(which(rollsum4sd > 17))],
            
            date_rollsum_plus_3 = time[min(which(rollsum3sd_plus > 3))],
            date_rollsum_plus_4 = time[min(which(rollsum3sd_plus > 4))],
            date_rollsum_plus_5 = time[min(which(rollsum3sd_plus > 5))],
            date_rollsum_plus_6 = time[min(which(rollsum3sd_plus > 6))],
            date_rollsum_plus_7 = time[min(which(rollsum3sd_plus > 7))],
            date_rollsum_plus_8 = time[min(which(rollsum3sd_plus > 8))],
            date_rollsum_plus_9 = time[min(which(rollsum3sd_plus > 9))],
            date_rollsum_plus_10 = time[min(which(rollsum3sd_plus > 10))],
            date_rollsum_plus_11 = time[min(which(rollsum3sd_plus > 11))],
            date_rollsum_plus_12 = time[min(which(rollsum3sd_plus > 12))],
            date_rollsum_plus_13 = time[min(which(rollsum3sd_plus > 13))],
            date_rollsum_plus_14 = time[min(which(rollsum3sd_plus > 14))],
            date_rollsum_plus_15 = time[min(which(rollsum3sd_plus > 15))],
            date_rollsum_plus_16 = time[min(which(rollsum3sd_plus > 16))],
            date_rollsum_plus_17 = time[min(which(rollsum3sd_plus > 17))],
            
            # date_rollsum4_plus_3 = time[min(which(rollsum4sd_plus > 3))],
            # date_rollsum4_plus_4 = time[min(which(rollsum4sd_plus > 4))],
            # date_rollsum4_plus_5 = time[min(which(rollsum4sd_plus > 5))],
            # date_rollsum4_plus_6 = time[min(which(rollsum4sd_plus > 6))],
            # date_rollsum4_plus_7 = time[min(which(rollsum4sd_plus > 7))],
            # date_rollsum4_plus_8 = time[min(which(rollsum4sd_plus > 8))],
            # date_rollsum4_plus_9 = time[min(which(rollsum4sd_plus > 9))],
            # date_rollsum4_plus_10 = time[min(which(rollsum4sd_plus > 10))],
            # date_rollsum4_plus_11 = time[min(which(rollsum4sd_plus > 11))],
            
            # date_rollsum5_plus_3 = time[min(which(rollsum5sd_plus > 3))],
            # date_rollsum5_plus_4 = time[min(which(rollsum5sd_plus > 4))],
            # date_rollsum5_plus_5 = time[min(which(rollsum5sd_plus > 5))],
            # date_rollsum5_plus_6 = time[min(which(rollsum5sd_plus > 6))],
            # date_rollsum5_plus_7 = time[min(which(rollsum5sd_plus > 7))],
            # date_rollsum5_plus_8 = time[min(which(rollsum5sd_plus > 8))],
            # date_rollsum5_plus_9 = time[min(which(rollsum5sd_plus > 9))],
            # date_rollsum5_plus_10 = time[min(which(rollsum5sd_plus > 10))],
            # date_rollsum5_plus_11 = time[min(which(rollsum5sd_plus > 11))],
            
            date_rollsum_minus_3 = time[min(which(rollsum3sd_minus > 3))],
            date_rollsum_minus_4 = time[min(which(rollsum3sd_minus > 4))],
            date_rollsum_minus_5 = time[min(which(rollsum3sd_minus > 5))],
            date_rollsum_minus_6 = time[min(which(rollsum3sd_minus > 6))],
            date_rollsum_minus_7 = time[min(which(rollsum3sd_minus > 7))],
            date_rollsum_minus_8 = time[min(which(rollsum3sd_minus > 8))],
            date_rollsum_minus_9 = time[min(which(rollsum3sd_minus > 9))],
            date_rollsum_minus_10 = time[min(which(rollsum3sd_minus > 10))],
            date_rollsum_minus_11 = time[min(which(rollsum3sd_minus > 11))],
            date_rollsum_minus_12 = time[min(which(rollsum3sd_minus > 12))],
            date_rollsum_minus_13 = time[min(which(rollsum3sd_minus > 13))]
            
            # date_rollsum4_minus_3 = time[min(which(rollsum4sd_minus > 3))],
            # date_rollsum4_minus_4 = time[min(which(rollsum4sd_minus > 4))],
            # date_rollsum4_minus_5 = time[min(which(rollsum4sd_minus > 5))],
            # date_rollsum4_minus_6 = time[min(which(rollsum4sd_minus > 6))],
            # date_rollsum4_minus_7 = time[min(which(rollsum4sd_minus > 7))],
            # date_rollsum4_minus_8 = time[min(which(rollsum4sd_minus > 8))],
            # date_rollsum4_minus_9 = time[min(which(rollsum4sd_minus > 9))],
            # date_rollsum4_minus_10 = time[min(which(rollsum4sd_minus > 10))],
            # date_rollsum4_minus_11 = time[min(which(rollsum4sd_minus > 11))]
            
            # date_rollsum5_minus_3 = time[min(which(rollsum5sd_minus > 3))],
            # date_rollsum5_minus_4 = time[min(which(rollsum5sd_minus > 4))],
            # date_rollsum5_minus_5 = time[min(which(rollsum5sd_minus > 5))],
            # date_rollsum5_minus_6 = time[min(which(rollsum5sd_minus > 6))],
            # date_rollsum5_minus_7 = time[min(which(rollsum5sd_minus > 7))],
            # date_rollsum5_minus_8 = time[min(which(rollsum5sd_minus > 8))],
            # date_rollsum5_minus_9 = time[min(which(rollsum5sd_minus > 9))],
            # date_rollsum5_minus_10 = time[min(which(rollsum5sd_minus > 10))],
            # date_rollsum5_minus_11 = time[min(which(rollsum5sd_minus > 11))]

            
            # date_rollsum6_plus_3 = time[min(which(rollsum6sd_plus > 3))],
            # date_rollsum6_plus_4 = time[min(which(rollsum6sd_plus > 4))],
            # date_rollsum6_plus_5 = time[min(which(rollsum6sd_plus > 5))],
            # date_rollsum6_plus_6 = time[min(which(rollsum6sd_plus > 6))],
            # date_rollsum6_plus_7 = time[min(which(rollsum6sd_plus > 7))],
            # date_rollsum6_plus_8 = time[min(which(rollsum6sd_plus > 8))],
            # date_rollsum6_plus_9 = time[min(which(rollsum6sd_plus > 9))],
            # date_rollsum6_plus_10 = time[min(which(rollsum6sd_plus > 10))],
            # date_rollsum6_plus_11 = time[min(which(rollsum6sd_plus > 11))]
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
  
  if (grepl("rollsum", names(alarm_df[i]))) {
    if (grepl("plus", names(alarm_df[i]))) {
      alarm = "roll_plus"
    } else if (grepl("minus", names(alarm_df[i]))) { 
      alarm = "roll_minus" 
    } else {alarm = "rollsum"}
  } else {alarm = "simple"}
  ROC = rbind(ROC, data.frame(FPR, TPR, above = names(alarm_df[i]), alarm = alarm))
}


ggplot(ROC, aes(FPR, TPR, col = alarm)) + geom_point() +  # , col = name in aes()
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_30_rollsums_added.png", width = 6.5, height = 5)

# save methods -------------------------------------------------------------------

#ROC$method = "interp_later"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_interp_later_rollsum.RData")

#ROC$method = "only_spline"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_only_spline_rollsum.RData")

# Compare interp_later and only_spline ---------------------------------------------------
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_only_spline_rollsum.RData")
ROC_methods = ROC
load("ROC_score_interp_later_rollsum.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$alarm = factor(ROC_methods$alarm, levels = unique(ROC_methods$alarm)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = alarm)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)+ # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

#setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_interp_later_only_spline_rollsum.png", width = 7, height = 3.4)


# final compare ---------------------------------------------------------------------
# ROC$method = "interp_later"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_non_rollsum.RData")

# ROC$method = "only_spline"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_non_rollsum.RData")

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_only_spline_non_rollsum.RData")
ROC_methods = ROC
load("ROC_score_interp_later_non_rollsum.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$alarm = factor(ROC_methods$alarm, levels = unique(ROC_methods$alarm)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = alarm)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)+ # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_interp_later_only_spline_non_rollsum.png", width = 7, height = 3.4)



# Final with learn_max --------------------------------------------------------------

# ROC$method = "learn_max"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_learn_max.RData")

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_non_rollsum.RData")
ROC = ROC[,c(1,2,3,5)]
ROC_methods = ROC
load("ROC_score_learn_max.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$alarm = factor(ROC_methods$alarm, levels = unique(ROC_methods$alarm)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = method)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) # + scale_color_hue(h = c(0,360), drop = FALSE)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_interp_later_only_spline_non_rollsum_learn_max.png", width = 6.5, height = 5)

