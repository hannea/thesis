#=========================================================================================
# Description: evaluation "before date" including alarm function and ROC plot
#
# Load: Output from models on all cases
# Save: ROC plot
#=========================================================================================
library(dplyr)
library(ggplot2)

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178.RData") # first
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_abs_res.RData") # abs_res
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res.RData") # interp_later
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30.RData")
## can not be handled as the other
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_learn_max.RData") # learn_max

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline.RData") # only_spline
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_30.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_30.RData")

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30_subset_by_power.RData")
## sensors
load("~/aspeciale/R_data_local/all_cases_RData/sensor_names.RData")
inner_join(df_loop_sd, sensor_df)

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_sqrt.RData") 
#interp_later_sqrt
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_no_transform.RData") 
#only_spline_non

# ROC figures -----------------------------------------------------------------------

# before date -----------------------------------------------------------------------

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
            date_11sd = time[min(which(abs_sd > 11))],
            
            # date_3sd_plus = time[min(which(sd > 3))],
            # date_4sd_plus = time[min(which(sd > 4))],
            # date_5sd_plus = time[min(which(sd > 5))],
            # date_6sd_plus = time[min(which(sd > 6))],
            # date_7sd_plus = time[min(which(sd > 7))],
            # date_8sd_plus = time[min(which(sd > 8))],
            # date_9sd_plus = time[min(which(sd > 9))],
            # date_10sd_plus = time[min(which(sd > 10))],
            # date_11sd_plus = time[min(which(sd > 11))],
            
            ## 2 in a row above ?sd
            date_2x3sd = time[min(which((abs_sd[-1] > 3 & abs_sd[-length(abs_sd)] > 3)))],
            date_2x4sd = time[min(which((abs_sd[-1] > 4 & abs_sd[-length(abs_sd)] > 4)))],
            date_2x5sd = time[min(which((abs_sd[-1] > 5 & abs_sd[-length(abs_sd)] > 5)))],
            date_2x6sd = time[min(which((abs_sd[-1] > 6 & abs_sd[-length(abs_sd)] > 6)))],
            date_2x7sd = time[min(which((abs_sd[-1] > 7 & abs_sd[-length(abs_sd)] > 7)))],
            date_2x8sd = time[min(which((abs_sd[-1] > 8 & abs_sd[-length(abs_sd)] > 8)))],
            
            # date_2_sign_4 = time[min(which((sd[-1] > 4 & sd[-length(abs_sd)] > 4) | 
            #                                  (sd[-1] < -4 & sd[-length(abs_sd)] < -4) ))],
            # date_2_sign_5 = time[min(which((sd[-1] > 5 & sd[-length(abs_sd)] > 5) | 
            #                                  (sd[-1] < -5 & sd[-length(abs_sd)] < -5) ))],
            # date_2_sign_6 = time[min(which((sd[-1] > 6 & sd[-length(abs_sd)] > 6) | 
            #                                  (sd[-1] < -6 & sd[-length(abs_sd)] < -6) ))],

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

#load("~/aspeciale/R_data_local/all_cases_RData/meta_tot_df.RData")
#obs = as.POSIXct(meta_df$knownalarm, tz = "GMT")

load("~/aspeciale/R_data_local/all_cases_RData/meta_u174_df.RData")
obs = as.POSIXct(meta_u174$knownalarm, tz = "GMT")

# ggplot ----------------------------------------------------------------------------
ROC = NULL

for(i in 3:length(alarm_df)){
  pred_time = alarm_df[[i]]
  TPR = sum(pred_time <= (obs[1:length(pred_time)] + 3600*24*3), na.rm = TRUE)/
    sum(!is.na(obs[1:length(pred_time)])) # + 3 days. TP/P ,  P = TP + FN
  FPR = sum((!is.na(pred_time)) > (!is.na(obs[1:length(pred_time)])))/
    sum(is.na(obs[1:length(pred_time)])) # FP/N ,  N = FP + TN
  ROC = rbind(ROC, data.frame(FPR, TPR, above = names(alarm_df[i])))
}


ggplot(ROC, aes(FPR, TPR, col = above)) + geom_point() +  # , col = name in aes()
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)

# save df ROC -----------------------------------------------------------------------

## !!!! only once !!!! ##
#ROC$method = "first"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_first.RData")

#ROC$method = "abs_res"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_abs_res.RData")

#ROC$method = "interp_later"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_interp_later.RData")

#ROC$method = "interp_later_30"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_interp_later_30.RData")

#ROC$method = "learn_max"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_learn_max.RData")

#ROC$method = "only_spline"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_only_spline.RData")

#ROC$method = "only_spline_30"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_only_spline_30.RData")

#ROC$method = "only_spline_abs"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_only_spline_abs_res.RData")

#ROC$method = "only_spline_abs_30"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_only_spline_abs_res_30.RData")

#ROC$method = "only_spline_sqrt"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_only_spline_sqrt.RData")

#ROC$method = "interp_later_sqrt"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_interp_later_sqrt.RData")

#ROC$method = "only_spline_non"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_date_only_spline_non.RData")

#ROC$method = "subset_by_power"

load("ROC_date_learn_max.RData")
names(ROC)[3:4] = c("above")
#ROC$method = "learn_max_date"
#save(ROC, file = "ROC_date_learn_max.RData")

# Several methods -------------------------------------------------------------------
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_date_first.RData")
ROC_methods = ROC
load("ROC_date_abs_res.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_date_interp_later.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_date_interp_later_30.RData")
#ROC$method = "before_date"
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_learn_max.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_date_learn_max.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_date_only_spline.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_date_only_spline_abs_res.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_date_only_spline_abs_res_30.RData")
ROC_methods = rbind(ROC_methods, ROC)

load("ROC_only_spline_30.RData")
ROC_methods = rbind(ROC_methods, ROC)

load("ROC_date_only_spline_sqrt.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$method = factor(ROC_methods$method, levels = unique(ROC_methods$method)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = method)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) # + scale_color_hue(h = c(0,360), drop = FALSE)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_learn_max_date.png", width = 6.5, height = 5)
#ggsave("ROC_first_abs_res_interp_later_learn_max_only_spline.png", width = 6.5, height = 5)
#ggsave("ROC_only_spline_vs_30.png", width = 6.5, height = 5)

-log(3)/10
x = -60:60
beta = 5
# ca 5 dage før svarer til 75 % og 5 dage efter til 25 %.
plot(x, exp(-x/beta)/(1+exp(-x/beta)), type = "l")


# Compare before date and no date and score ----------------------------------------------
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_only_spline.RData")
ROC$alarm = "no date"
ROC_methods = ROC

load("ROC_date_only_spline.RData")
ROC$alarm = "before date"
ROC_methods = rbind(ROC_methods, ROC)

load("ROC_interp_later.RData")
ROC$alarm = "no date"
ROC_methods = rbind(ROC_methods, ROC)

load("ROC_date_interp_later.RData")
ROC$alarm = "before date"
ROC_methods = rbind(ROC_methods, ROC)

load("ROC_date_interp_later_score.RData")

ROC$method = "interp_later"
ROC$method = "only_spline"

ROC$alarm = "score"

ROC_methods = rbind(ROC_methods, ROC)
load("ROC_date_only_spline_score.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$alarm = factor(ROC_methods$alarm, levels = unique(ROC_methods$alarm)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = alarm)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)+ # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_interp_later_only_spline_no_date_vs_before_date.png", width = 7, height = 3.4)
