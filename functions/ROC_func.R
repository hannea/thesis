##########################################################################################
## ROC_func
##########################################################################################

ROC_func = function(alarm_df, obs, plot = TRUE) {
  ROC = NULL
  
  for(i in 3:length(alarm_df)){
    pred_time = alarm_df[[i]]
    TPR = sum(pred_time <= (obs[1:length(pred_time)] + 3600*24*3), na.rm = TRUE)/
      sum(!is.na(obs[1:length(pred_time)])) # + 3 days. TP/P
    FPR = sum((!is.na(pred_time)) > (!is.na(obs[1:length(pred_time)])))/
      sum(is.na(obs[1:length(pred_time)])) # FP/N
    ROC = rbind(ROC, data.frame(FPR, TPR, name = names(alarm_df[i])))
  }
  
  if (plot == TRUE) {
    require(ggplot2)
    print(ggplot(ROC, aes(FPR, TPR)) + geom_point() +         # col = name
      ggtitle("ROC") + 
      coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
      geom_abline(intercept = 0, slope = 1))
  }
  return(ROC)
}



