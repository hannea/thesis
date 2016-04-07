# function that takes a training and test set of load and vibrations. So far it 
# is still neccessary to define the knots manually.


count_alarms_constant_var_vectorize = function(load_training,vib_training,knots=NULL,
                                     load_test,vib_test){
  par(mar=c(4,4,2,1))
  plot(load_training, log(vib_training), main="Constant Variance", pch=20,  col=rgb(0,0,0,0.02),
       xlab="Load", ylab="Vibrations")
  abline(v = knots, col = "darkgray", lty = 3)
  
  require(splines)
  ns_object = ns(c(min(load_training,na.rm = TRUE),
                   max(load_training, na.rm = TRUE)),knots = knots)
  fit_object = lm(log(vib_training) ~ predict(ns_object,load_training))
  coef = matrix(c(coef(fit_object)),ncol=1)
  pred_test = cbind(rep(1,length(load_test)),predict(ns_object,load_test)) %*% coef # 4832x3   %*%  5x1
  sum = summary(fit_object)
  sigma = sum$sigma
  
  fit = lm(log(vib_training) ~ ns(load_training, knots = knots))
  load_vec = seq(ceiling(min(load_training[!is.na(load_training)])), 
                 floor(max(load_training[!is.na(load_training)])), length.out = 100)
  pred = predict(fit, data.frame(load_training = load_vec), se.fit = T)
  lines(load_vec, predict(fit, data.frame(load_training=load_vec)), lwd=2, col="red")
  lines(load_vec , pred$fit +3* sigma ,lty ="dashed", col = "red")
  lines(load_vec , pred$fit -3* sigma ,lty ="dashed", col = "red")
  legend("bottomright", c("spline","3 sigma band", "test set outside band"), lwd = c(1,1,1), 
         col = c("red","red","green"), lty = c(1,2,0), pch = c(NA,NA,20), cex = 0.8)
  
  which_outside = log(vib_test) < pred_test - 3*sigma | log(vib_test) > pred_test + 3*sigma 
  count = sum(which_outside)
  points(load_test[which_outside], log(vib_test[which_outside]), col = "green", pch = 20)
  
  alarm_rate = count/length(load_test)
  
  out = list("count" = count, "alarm_rate" = alarm_rate)
  return(out)
}