#=========================================================================================
# Description: evaluation "simple" including alarm function and ROC plot
#
# Load: Output from models on all cases
# Save: ROC plot
#=========================================================================================

library(dplyr)
library(ggplot2)


load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178.RData") # first
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_abs_res.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30.RData")

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30_1000.RData")
## can not be handled as the other
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_learn_max.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline.RData")
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_30_knots.RData")

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_sqrt.RData") 
load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_sqrt.RData") 
# ROC figures -----------------------------------------------------------------------

# date or no date -------------------------------------------------------------------
alarm_df =
  df_loop_sd %>% 
  group_by(index) %>% 
  filter(1:length(sd) > 50 & 1:length(sd) < (length(sd)-50) , !is.na(sd)) %>% 
  mutate(abs_sd = abs(sd)) %>% 
  summarise(n = n(), 
            
            ## above ?sd
            n_3sd = sum(abs_sd > 3),
            n_4sd = sum(abs(sd) > 4),
            n_5sd = sum(abs(sd) > 5),
            n_6sd = sum(abs(sd) > 6),
            n_7sd = sum(abs(sd) > 7),
            n_8sd = sum(abs(sd) > 8),
            n_9sd = sum(abs(sd) > 9),
            n_10sd = sum(abs(sd) > 10),
            #n_11sd = sum(abs(sd) > 11),
            
            ## 2 in a row above ?sd
            n_2x3sd = sum(abs_sd[-1] > 3 & abs_sd[-length(abs_sd)] > 3),
            n_2x4sd = sum(abs_sd[-1] > 4 & abs_sd[-length(abs_sd)] > 4),
            n_2x5sd = sum(abs_sd[-1] > 5 & abs_sd[-length(abs_sd)] > 5),
            n_2x6sd = sum(abs_sd[-1] > 6 & abs_sd[-length(abs_sd)] > 6),
            n_2x7sd = sum(abs_sd[-1] > 7 & abs_sd[-length(abs_sd)] > 7),
            n_2x8sd = sum(abs_sd[-1] > 8 & abs_sd[-length(abs_sd)] > 8),
            
            ## 3 in a row above ?sd
            #n_3x2sd = sum(abs_sd[-(1:2)] > 2 & abs_sd[-c(1,length(abs_sd))] > 2 & 
            #                abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 2),
            n_3x3sd = sum(abs_sd[-(1:2)] > 3 & abs_sd[-c(1,length(abs_sd))] > 3 & 
                            abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 3),
            n_3x4sd = sum(abs_sd[-(1:2)] > 4 & abs_sd[-c(1,length(abs_sd))] > 4 & 
                            abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 4),
            n_3x5sd = sum(abs_sd[-(1:2)] > 5 & abs_sd[-c(1,length(abs_sd))] > 5 & 
                            abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 5)
  )

load("~/aspeciale/R_data_local/all_cases_RData/meta_tot_df.RData")

obs = meta_df$knownalarm
obs[!is.na(obs)]=1
obs[is.na(obs)]=0
obs = as.numeric(obs)

library(SDMTools)

# ggplot ----------------------------------------------------------------------------
ROC = NULL

for(i in 3:length(alarm_df)){
  pred = alarm_df[[i]]
  pred[pred>0]=1
  conf_matrix = confusion.matrix(obs[1:length(pred)],pred)
  TPR = sensitivity(conf_matrix) # TPR
  FPR = 1 - specificity(conf_matrix) # FPR
  ROC = rbind(ROC, data.frame(FPR, TPR, above = names(alarm_df[i])))
}

ggplot(ROC, aes(FPR, TPR, col = above)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3)

#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")

#setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_df_loop_sd_178.png", width = 6.1, height = 5) # to get same size as others with methods
#ggsave("ROC_df_loop_sd_178_abs_res.png", width = 6.5, height = 5)


# save df ROC -----------------------------------------------------------------------

## !!!! only once !!!! ##
#ROC$method = "first"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_first.RData")

#ROC$method = "abs_res"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_abs_res.RData")

#ROC$method = "interp_later"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_interp_later.RData")

#ROC$method = "interp_later_30"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_interp_later_30.RData")

#ROC$method = "interp_later_30_1000"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_interp_later_30_1000.RData")

#ROC$method = "learn_max"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_learn_max.RData")

#ROC$method = "only_spline"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_only_spline.RData")

#ROC$method = "only_spline_30"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_only_spline_30.RData")

#ROC$method = "interp_later_sqrt"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_interp_later_sqrt.RData")

#ROC$method = "only_spline_sqrt"
#setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_only_spline_sqrt.RData")

# Several methods -------------------------------------------------------------------
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
load("ROC_first.RData")
ROC_methods = ROC
load("ROC_abs_res.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_interp_later.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_learn_max.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_only_spline.RData")
ROC_methods = rbind(ROC_methods, ROC)

load("ROC_only_spline_30.RData")
ROC_methods = rbind(ROC_methods, ROC)

load("ROC_interp_later_30.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_interp_later_30_1000.RData")
ROC_methods = rbind(ROC_methods, ROC)

load("ROC_interp_later_sqrt.RData")
ROC_methods = rbind(ROC_methods, ROC)
load("ROC_only_spline_sqrt.RData")
ROC_methods = rbind(ROC_methods, ROC)

ROC_methods$method = factor(ROC_methods$method, levels = unique(ROC_methods$method)) #keep same order
                                                                                     #otherwise alphabetical
ggplot(ROC_methods, aes(FPR, TPR, col = method)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.3) # + scale_color_hue(h = c(0,360), drop = FALSE)

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("ROC_first_abs_res.png", width = 6.5, height = 5)
#ggsave("ROC_first_abs_res_interp_later.png", width = 6.5, height = 5)
#ggsave("ROC_interp_later_learn_max_only_spline.png", width = 6.5, height = 5)

#ggsave("ROC_abs_res_interp_later_learn_max_only_spline.png", width = 6.5, height = 5)
#ggsave("ROC_first_abs_res_interp_later_learn_max_only_spline.png", width = 6.5, height = 5)


#ggsave("ROC_only_spline_vs_30.png", width = 6.5, height = 5)
#ggsave("ROC_after_date_vs_no_date.png", width = 6.5, height = 5)
#ggsave("ROC_abs_res_interp_later_vs_30.png", width = 6.5, height = 5)
