#=========================================================================================
# Description: "learn_max" monitor model
#
# Load: all cases as RData files
# Save: a data frame containing times in test data, the maximum values and
#       the case index
#=========================================================================================
library(dplyr)
library(ggplot2)


df_learn_max = NULL

for (i in 0:178){
  print(i)
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  load(bl_file_name)
  m_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_m.RData', sep = "")
  load(m_file_name)

  # Identify --------------------------------------------------------------------------
  bl = tbl_df(bl)
  bl[[1]] = as.POSIXct(bl[[1]], tz = "GMT")
  
  m = tbl_df(m)
  m[[1]] = as.POSIXct(m[[1]], tz = "GMT")

  # Training --------------------------------------------------------------------------
  max_train = max(bl[[2]])

  
  # Testing ---------------------------------------------------------------------------
  max = m[[2]] > max_train


  df_learn_max = rbind(df_learn_max, data.frame(time = m[[1]], max = max, index = i))
}
#setwd("~/aspeciale/R_data_local")
setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#save(df_learn_max, file = "df_loop_sd_1_learn_max.RData")


