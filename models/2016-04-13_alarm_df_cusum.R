#=========================================================================================
# Description: Joins the output of the monitor model to new columns calculated using
# CUSUMs 
#
# Load: output from monitor models
# Save: joined dataframe with extra columns
#=========================================================================================

load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res.RData") 
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_interp_later_abs_res_30.RData") # interp_later
##load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline.RData") # only_spline
#load("~/aspeciale/R_data_local/plot_df_loop_sd/df_loop_sd_178_only_spline_30.RData") # only_spline

# load("~/documents/saved/df_loop_sd_178_interp_later_abs_res_30.RData")


# CUSUM -----------------------------------------------------------------------------

Rprof()
df_sub = df_loop_sd[2:3] # sd and index
for (k in 1:3) { # 1:5
  cat("k = ", k,"\n")
  cusum_U_tot = NULL
  cusum_L_tot = NULL
  
  ## Loop over all cases
  for (index in 0:178) {
    print(index)
    df_filter = "[.data.frame"(df_sub, which(df_sub$index == index), T) # to speed up code
    #df_filter = df_sub[df_sub$index == index,]
    ts = df_filter$sd
    
    ## Upper CUSUM
    diff_U = ts - k   # Upper
    diff_U[is.na(diff_U)] = 0
    S_temp_U = 0
    cusum_U = NULL
    
    ## Lower CUSUM
    diff_L = -ts - k # Lower, -ts to get same formula as for upper.
    diff_L[is.na(diff_L)] = 0
    S_temp_L = 0
    cusum_L = NULL
    
    ## Loop over all observations for each case
    for (i in 1:length(ts)) {
      S_temp_U = max(S_temp_U + diff_U[i], 0)
      cusum_U[i] = S_temp_U
      
      S_temp_L = max(S_temp_L + diff_L[i], 0)
      cusum_L[i] = S_temp_L
    }
    cusum_U_tot = c(cusum_U_tot, cusum_U)
    cusum_L_tot = c(cusum_L_tot, cusum_L)
  }
  nameU = paste0("cusum_U", k)
  df_loop_sd[nameU] = cusum_U_tot
  
  nameL = paste0("cusum_L", k)
  df_loop_sd[nameL] = cusum_L_tot
}
Rprof(NULL)
summaryRprof()


setwd("~/aspeciale/R_data_local/plot_df_loop_sd")
#setwd("~/documents/saved")
##save(df_loop_sd, file = "df_loop_sd_178_interp_later_abs_res_cusum_added_LU.RData")

#save(df_loop_sd, file = "df_loop_sd_178_interp_later_abs_res_30_cusum_added.RData")
##save(df_loop_sd, file = "df_loop_sd_178_only_spline_cusum_added.RData")
#save(df_loop_sd, file = "df_loop_sd_178_only_spline_30_cusum_added.RData")
