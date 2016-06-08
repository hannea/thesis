#=========================================================================================
# Description: evaluation "score" including alarm function and ROC plot
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

## transforms
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_sqrt.RData")#interp_l_sqrt
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_sqrt.RData") #only_s_sqrt

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non_10.RData") #not same below
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non.RData") #interp_l_non (abs)
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_no_transform.RData") #only_s_non (quan)(q10)

## spline fit to residuals: abs, pos, or quan (and two above)
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non.RData") #only_spline (abs) (10)
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_pos_res_non.RData") #only_spline (pos)
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_non_pos_res.RData") #interp_later (pos)
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_quantile.RData") #interp_later (quan) (q10)

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_abs_res_auto_arima_non.RData")

## knots 10 and 30
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_no_transform_30.RData") 
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_no_transform_50.RData") 
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_quantile_30.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_quantile_50.RData")

## using abs_res knots 20 and 30
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non_20.RData") # 20
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non_30.RData") # 30
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non_20.RData") # only_s_20
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non_30.RData") # only_s_30

## vzero, using 10 knots, abs_res and non
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non_vzero.RData") # only_s_vzero
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non_vzero.RData") # interp_l_vzero

## color by sensors
#load("~/aspeciale/R_data_local/all_cases_RData/sensor_names.RData")
#inner_join(df_loop_sd, sensor_df)

## subset by power

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non_10_subset_by_power.RData") #p100
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non_10_subset_by_power_p10.RData")

df_loop_sd = filter(df_loop_sd, index != 174)

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_non_30_subset_by_power.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_abs_res_non_30_subset_by_power.RData")

# special for subset by power
load("~/aspeciale/R_data_local/all_cases_RData/meta_u174_df.RData")
obs = as.POSIXct(meta_u174$knownalarm, tz = "GMT")

## auto.arima
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_abs_res_auto_arima_non.RData")

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
            date_2x9sd = time[min(which((abs_sd[-1] > 9 & abs_sd[-length(abs_sd)] > 9)))],
            date_2x10sd = time[min(which((abs_sd[-1] > 10 & abs_sd[-length(abs_sd)] > 10)))],

            ## 3 in a row above ?sd
            date_3x3 = time[min(which((abs_sd[-(1:2)] > 3 & abs_sd[-c(1,length(abs_sd))] > 3 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 3)))],            
            date_3x4 = time[min(which((abs_sd[-(1:2)] > 4 & abs_sd[-c(1,length(abs_sd))] > 4 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 4)))],
            date_3x5 = time[min(which((abs_sd[-(1:2)] > 5 & abs_sd[-c(1,length(abs_sd))] > 5 & 
                                         abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 5)))]
  )

load("~/aspeciale/R_data_local/all_cases_RData/meta_tot_df.RData")
obs = as.POSIXct(meta_df$knownalarm, tz = "GMT")


# ggplot ----------------------------------------------------------------------------
ROC = NULL

for(i in 3:length(alarm_df)){
  pred_time = alarm_df[[i]]
  # time difference 1/(1+exp(-timediff/beta))
  dt = difftime(pred_time, obs, units = "days")
  scored = sapply(dt, function(x) 1/(1 + exp((0.22*x-0)))) # score all the. minus the 50 % center, eg. 10 days
  TPR = sum(scored, na.rm = TRUE)/ sum(!is.na(obs[1:length(pred_time)]))
  FPR = sum((!is.na(pred_time)) > (!is.na(obs[1:length(pred_time)])))/
    sum(is.na(obs[1:length(pred_time)])) # FP/N ,  N = FP + TN
  ROC = rbind(ROC, data.frame(FPR, TPR, above = names(alarm_df[i])))
}


ggplot(ROC, aes(FPR, TPR, col = above)) + geom_point() +  # , col = name in aes()
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)

# Compare several transforms -------------------------------------------------------------

#ROC$method = "interp_later"
#ROC$transform = "log"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_interp_later_log.RData")

#ROC$method = "only_spline"
#ROC$transform = "log"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_only_spline_log.RData")

#ROC$method = "interp_later"
#ROC$transform = "sqrt"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_interp_later_sqrt.RData")

#ROC$method = "only_spline"
#ROC$transform = "sqrt"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_only_spline_sqrt.RData")

# ROC$method = "interp_later"
# ROC$transform = "non"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_non.RData")

#ROC$method = "only_spline"
#ROC$transform = "non"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_score_only_spline_non.RData")


setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_non.RData")
ROC_methods = ROC
load("ROC_score_only_spline_non.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_sqrt.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_sqrt.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_log.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_log.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$transform = factor(ROC_methods$transform, levels = unique(ROC_methods$transform)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = transform)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) + # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_non_sqrt_log.png", width = 7, height = 3.4)

# Compare pos res with abs_res and quantile all non ---------------------------------------------------

# ROC$method = "interp_later"
# ROC$residual = "quan"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_quan.RData")

# ROC$method = "only_spline"
# ROC$residual = "quan"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_quan.RData")

# ROC$method = "interp_later"
# ROC$residual = "abs"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_abs.RData")

# ROC$method = "only_spline"
# ROC$residual = "abs"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_abs.RData")

# ROC$method = "interp_later"
# ROC$residual = "pos"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_pos.RData")

# ROC$method = "only_spline" # not 10 knots
# ROC$residual = "pos"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_pos.RData")

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_quan.RData") # missing
ROC_methods = ROC
load("ROC_score_only_spline_quan.RData") #old
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_abs.RData") #old
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_abs.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_pos.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_pos.RData")
ROC_methods = rbind(ROC_methods, ROC)


ROC_methods$residual = factor(ROC_methods$residual, levels = unique(ROC_methods$residual)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = residual)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) + # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
ggsave("ROC_quan_abs_pos.png", width = 7, height = 3.4)

# knots using quantile (use abs_res instead) ---------------------------------------------
# ROC$method = "interp_later"
# ROC$knots = 10
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_knots_10.RData")

# ROC$method = "only_spline"
# ROC$knots = 10
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_knots_10.RData")

# ROC$method = "interp_later"
# ROC$knots = 30
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_knots_30.RData")

# ROC$method = "only_spline"
# ROC$knots = 30
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_knots_30.RData")

# ROC$method = "interp_later"
# ROC$knots = 50
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_knots_50.RData")

# ROC$method = "only_spline"
# ROC$knots = 50
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_knots_50.RData")

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_knots_10.RData")
ROC_methods = ROC
load("ROC_score_only_spline_knots_10.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_knots_30.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_knots_30.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_knots_50.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_knots_50.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$knots = factor(ROC_methods$knots, levels = unique(ROC_methods$knots)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = knots)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) + # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_knots.png", width = 7, height = 3.4)

# knots using abs_res instead ------------------------------------------------------------
# ROC$method = "interp_later"
# ROC$knots = 10
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_abs_knots_10.RData")

# ROC$method = "only_spline"
# ROC$knots = 10
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_abs_knots_10.RData")

# ROC$method = "interp_later"
# ROC$knots = 20
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_abs_knots_20.RData")

# ROC$method = "only_spline"
# ROC$knots = 20
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_abs_knots_20.RData")

# ROC$method = "interp_later"
# ROC$knots = 30
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_abs_knots_30.RData")

# ROC$method = "only_spline"
# ROC$knots = 30
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_abs_knots_30.RData")

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_abs_knots_10.RData")
ROC_methods = ROC
load("ROC_score_only_spline_abs_knots_10.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_abs_knots_20.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_abs_knots_20.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_abs_knots_30.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_abs_knots_30.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$knots = factor(ROC_methods$knots, levels = unique(ROC_methods$knots)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = knots)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) + # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_knots_abs.png", width = 7, height = 3.4)

# Subset by power - 10 knots! ------------------------------------------------------------
# ROC$method = "interp_later"
# ROC$subset = "by power"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_subset_power.RData")

# ROC$method = "only_spline"
# ROC$subset = "by power"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_subset_power.RData")

# ROC$method = "only_spline"
# ROC$subset = "by power 10"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_subset_power10.RData")

# ROC$method = "interp_later"
# ROC$subset = "by load"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_subset_load.RData")

# ROC$method = "only_spline"
# ROC$subset = "by load"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_subset_load.RData")

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_subset_power.RData")
ROC_methods = ROC
load("ROC_score_only_spline_subset_power.RData")
ROC_methods = rbind(ROC_methods, ROC)
#load("ROC_score_only_spline_subset_power10.RData")
#ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_subset_load.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_subset_load.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$subset = factor(ROC_methods$subset, levels = unique(ROC_methods$subset)) #keep same order
#otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = subset)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) + # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_subset_10_knots.png", width = 7, height = 3.4)

# vzero, Remove vib == 0, non, 10 knots ---------------------------------------------------------
# ROC$method = "interp_later"
# ROC$vib = "all"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_vib_all.RData")

# ROC$method = "only_spline"
# ROC$vib = "all"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_vib_all.RData")

# ROC$method = "interp_later"
# ROC$vib = "no zero"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_vib_zero.RData")

# ROC$method = "only_spline"
# ROC$vib = "no zero"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_only_spline_vib_zero.RData")

setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

load("ROC_score_interp_later_vib_all.RData")
ROC_methods = ROC
load("ROC_score_only_spline_vib_all.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_interp_later_vib_zero.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_score_only_spline_vib_zero.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$vib = factor(ROC_methods$vib, levels = unique(ROC_methods$vib)) #keep same order
ggplot(ROC_methods, aes(FPR, TPR, col = vib)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) + # + scale_color_hue(h = c(0,360), drop = FALSE)
  facet_wrap(~method)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_vzero.png", width = 7, height = 3.4)

# auto arima ------------------------------------------------------------------------
# ROC$method = "auto"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_auto_non.RData")

# ROC$method = "interp_later"
# setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
# save(ROC, file = "ROC_score_interp_later_compare.RData")

load("ROC_score_interp_later_compare.RData")
ROC_methods = ROC
load("ROC_score_auto_non.RData")
ROC_methods = rbind(ROC_methods, ROC)

ggplot(ROC_methods, aes(FPR, TPR, col = method)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_score_auto.png", width = 6.5, height = 5)