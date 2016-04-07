##########################################################################################
## Alarm_func
##########################################################################################

alarm_func = function(df_loop_sd, rollsum = FALSE, rollsum_sign = FALSE){
  require(dplyr)
  summa =
    suppressWarnings(df_loop_sd %>% 
    group_by(index) %>% 
    filter(1:length(sd) > 50 & 1:length(sd) < (length(sd)-50) , !is.na(sd)) %>% 
    mutate(abs_sd = abs(sd)) %>% 
    summarise(n = n(),
              ## these are not as good as rollsum
              date_3sd = time[min(which(abs_sd > 3))],
              date_4sd = time[min(which(abs_sd > 4))],
              date_5sd = time[min(which(abs_sd > 5))],
              date_6sd = time[min(which(abs_sd > 6))],
              date_7sd = time[min(which(abs_sd > 7))],
              date_8sd = time[min(which(abs_sd > 8))],
              date_9sd = time[min(which(abs_sd > 9))],
              date_10sd = time[min(which(abs_sd > 10))],
              date_11sd = time[min(which(abs_sd > 11))],
              
              date_3sd_minus = time[min(which(sd < -3))],
              date_4sd_minus = time[min(which(sd < -4))],
              date_5sd_minus = time[min(which(sd < -5))],
              date_6sd_minus = time[min(which(sd < -6))],
              date_7sd_minus = time[min(which(sd < -7))],
              date_8sd_minus = time[min(which(sd < -8))],
              date_9sd_minus = time[min(which(sd < -9))],
              date_10sd_minus = time[min(which(sd < -10))],
              date_11sd_minus = time[min(which(sd < -11))],
              
              date_3sd_plus = time[min(which(sd > 3))],
              date_4sd_plus = time[min(which(sd > 4))],
              date_5sd_plus = time[min(which(sd > 5))],
              date_6sd_plus = time[min(which(sd > 6))],
              date_7sd_plus = time[min(which(sd > 7))],
              date_8sd_plus = time[min(which(sd > 8))],
              date_9sd_plus = time[min(which(sd > 9))],
              date_10sd_plus = time[min(which(sd > 10))],
              date_11sd_plus = time[min(which(sd > 11))],
              
              date_2x3sd = time[min(which((abs_sd[-1] > 3 & abs_sd[-length(abs_sd)] > 3)))],
              date_2x4sd = time[min(which((abs_sd[-1] > 4 & abs_sd[-length(abs_sd)] > 4)))],
              date_2x5sd = time[min(which((abs_sd[-1] > 5 & abs_sd[-length(abs_sd)] > 5)))],
              date_2x6sd = time[min(which((abs_sd[-1] > 6 & abs_sd[-length(abs_sd)] > 6)))],
              date_2x7sd = time[min(which((abs_sd[-1] > 7 & abs_sd[-length(abs_sd)] > 7)))],
              date_2x8sd = time[min(which((abs_sd[-1] > 8 & abs_sd[-length(abs_sd)] > 8)))],
              
              date_2_sign_4 = time[min(which((sd[-1] > 4 & sd[-length(abs_sd)] > 4) | 
                                               (sd[-1] < -4 & sd[-length(abs_sd)] < -4) ))],
              date_2_sign_5 = time[min(which((sd[-1] > 5 & sd[-length(abs_sd)] > 5) | 
                                               (sd[-1] < -5 & sd[-length(abs_sd)] < -5) ))],
              date_2_sign_6 = time[min(which((sd[-1] > 6 & sd[-length(abs_sd)] > 6) | 
                                               (sd[-1] < -6 & sd[-length(abs_sd)] < -6) ))],
              date_3x4 = time[min(which((abs_sd[-(1:2)] > 4 & abs_sd[-c(1,length(abs_sd))] > 4 & 
                                           abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 4)))],
              date_3x5 = time[min(which((abs_sd[-(1:2)] > 5 & abs_sd[-c(1,length(abs_sd))] > 5 & 
                                           abs_sd[-c((length(abs_sd)-1),length(abs_sd))] > 5)))]
  )) #%>% 
  return(summa)
}


#               if (rollsum == TRUE) {
#                 date_rollsum_3 = time[min(which(rollsum3sd > 3))],
#                 date_rollsum_4 = time[min(which(rollsum3sd > 4))],
#                 date_rollsum_5 = time[min(which(rollsum3sd > 5))],
#                 date_rollsum_6 = time[min(which(rollsum3sd > 6))],
#                 date_rollsum_7 = time[min(which(rollsum3sd > 7))],
#                 date_rollsum_8 = time[min(which(rollsum3sd > 8))],
#                 date_rollsum_9 = time[min(which(rollsum3sd > 9))],
#               }
#               
#               if (rollsum_sign == TRUE) {
#                 date_rollsum3_minus_3 = time[min(which(rollsum3sd_minus > 3))],
#                 date_rollsum3_minus_4 = time[min(which(rollsum3sd_minus > 4))],
#                 date_rollsum3_minus_5 = time[min(which(rollsum3sd_minus > 5))],
#                 date_rollsum3_minus_6 = time[min(which(rollsum3sd_minus > 6))],
#                 date_rollsum3_minus_7 = time[min(which(rollsum3sd_minus > 7))],
#                 date_rollsum3_minus_8 = time[min(which(rollsum3sd_minus > 8))],
#                 date_rollsum3_minus_9 = time[min(which(rollsum3sd_minus > 9))],
#                 date_rollsum3_minus_10 = time[min(which(rollsum3sd_minus > 10))],
#                 date_rollsum3_minus_11 = time[min(which(rollsum3sd_minus > 11))],
#                 
#                 date_rollsum3_plus_3 = time[min(which(rollsum3sd_plus > 3))],
#                 date_rollsum3_plus_4 = time[min(which(rollsum3sd_plus > 4))],
#                 date_rollsum3_plus_5 = time[min(which(rollsum3sd_plus > 5))],
#                 date_rollsum3_plus_6 = time[min(which(rollsum3sd_plus > 6))],
#                 date_rollsum3_plus_7 = time[min(which(rollsum3sd_plus > 7))],
#                 date_rollsum3_plus_8 = time[min(which(rollsum3sd_plus > 8))],
#                 date_rollsum3_plus_9 = time[min(which(rollsum3sd_plus > 9))],
#                 date_rollsum3_plus_10 = time[min(which(rollsum3sd_plus > 10))],
#                 date_rollsum3_plus_11 = time[min(which(rollsum3sd_plus > 11))],
#                 
#                 date_rollsum4_plus_3 = time[min(which(rollsum4sd_plus > 3))],
#                 date_rollsum4_plus_4 = time[min(which(rollsum4sd_plus > 4))],
#                 date_rollsum4_plus_5 = time[min(which(rollsum4sd_plus > 5))],
#                 date_rollsum4_plus_6 = time[min(which(rollsum4sd_plus > 6))],
#                 date_rollsum4_plus_7 = time[min(which(rollsum4sd_plus > 7))],
#                 date_rollsum4_plus_8 = time[min(which(rollsum4sd_plus > 8))],
#                 date_rollsum4_plus_9 = time[min(which(rollsum4sd_plus > 9))],
#                 date_rollsum4_plus_10 = time[min(which(rollsum4sd_plus > 10))],
#                 date_rollsum4_plus_11 = time[min(which(rollsum4sd_plus > 11))],
#                 
#                 date_rollsum5_plus_3 = time[min(which(rollsum5sd_plus > 3))],
#                 date_rollsum5_plus_4 = time[min(which(rollsum5sd_plus > 4))],
#                 date_rollsum5_plus_5 = time[min(which(rollsum5sd_plus > 5))],
#                 date_rollsum5_plus_6 = time[min(which(rollsum5sd_plus > 6))],
#                 date_rollsum5_plus_7 = time[min(which(rollsum5sd_plus > 7))],
#                 date_rollsum5_plus_8 = time[min(which(rollsum5sd_plus > 8))],
#                 date_rollsum5_plus_9 = time[min(which(rollsum5sd_plus > 9))],
#                 date_rollsum5_plus_10 = time[min(which(rollsum5sd_plus > 10))],
#                 date_rollsum5_plus_11 = time[min(which(rollsum5sd_plus > 11))],
#                 
#                 date_rollsum6_plus_3 = time[min(which(rollsum6sd_plus > 3))],
#                 date_rollsum6_plus_4 = time[min(which(rollsum6sd_plus > 4))],
#                 date_rollsum6_plus_5 = time[min(which(rollsum6sd_plus > 5))],
#                 date_rollsum6_plus_6 = time[min(which(rollsum6sd_plus > 6))],
#                 date_rollsum6_plus_7 = time[min(which(rollsum6sd_plus > 7))],
#                 date_rollsum6_plus_8 = time[min(which(rollsum6sd_plus > 8))],
#                 date_rollsum6_plus_9 = time[min(which(rollsum6sd_plus > 9))],
#                 date_rollsum6_plus_10 = time[min(which(rollsum6sd_plus > 10))],
#                 date_rollsum6_plus_11 = time[min(which(rollsum6sd_plus > 11))],
#               }
#     )
# }

#alarm_func = function(df_loop_sd, rollsum = FALSE, rollsum_sign = FALSE){
#  require(dplyr)
#  df_loop_sd %>% 
#    group_by(index) %>% 
#    filter(1:length(sd) > 50 & 1:length(sd) < (length(sd)-50) , !is.na(sd)) %>% 
#    mutate(abs_sd = abs(sd), abs_sd3 = abs_sd > 3, abs_sd4 = abs_sd > 4) %>% 
#    summarise_each(funs(time[min(which(.))]), abs_sd3, abs_sd4)
#}
# could add using mutate for all alarm types. And summarise using time[min(which(.))] in summarise_each