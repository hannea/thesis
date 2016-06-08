#=========================================================================================
# Description: evaluation script of alarm function and ROC plot
#
# Load: Output from learn_max model on all cases
# Save: ROC plot
#=========================================================================================

library(dplyr)
library(ggplot2)
library(zoo)
load("~/aspeciale/R_data_local/df_learn_max_178.RData")
head(df_learn_max)

#ggplot(df_loop_sd, aes(time, sd, col = factor(index))) + geom_point() + facet_grid(index~.)

# zoo_package -----------------------------------------------------------------------
## Run this !!!!
logic = df_learn_max$max
logic[is.na(logic)]=FALSE
sum_logic = rollapply(logic, 30, sum, fill = NA, align = "r", partial = TRUE) # sum(sum_logic>2)#same as loop above
df_learn_max$rollsum = sum_logic

## after rollsum added
alarm_df =
  df_learn_max %>% 
  group_by(index) %>% 
  filter(!is.na(max)) %>% 
  summarise(n = n(), 
            n_max = sum(max),
            n_max_2 = sum(max[-1] & max[-length(max)]),
            n_max_3 = sum(max[-(1:2)] & max[-c(1,length(max))] & 
                          max[-c((length(max)-1),length(max))]),
            n_max_4 = sum(max[-(1:3)] & max[-c(1,2,length(max))] & max[-c(1,(length(max)-1),length(max))] & 
                            max[-c((length(max)-2):length(max))])
  )
            
            n_rollsum2 = sum(rollsum>2),
            n_rollsum3 = sum(rollsum>3),
            n_rollsum4 = sum(rollsum>4),
            n_rollsum5 = sum(rollsum>5),
            n_rollsum6 = sum(rollsum>6),
            n_rollsum7 = sum(rollsum>7)
  )

load("~/aspeciale/R_data_local/all_cases_RData/meta_tot_df.RData")

obs = (meta_df$knownalarm)
obs[!is.na(obs)]=1
obs[is.na(obs)]=0
as.numeric(obs)

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

ROC$method = "learn_max"
#setwd("~/aspeciale/R_data_local/ROC_df")
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(ROC, file = "ROC_learn_max.RData")

ggplot(ROC, aes(FPR, TPR, col = above)) + geom_point() + 
  ggtitle("ROC") + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1)
