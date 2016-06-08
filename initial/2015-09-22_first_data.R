#=========================================================================================
# Description: Initial play with data, and testing the package dlm
#
# Load: "datasubset.RData"
#=========================================================================================

setwd("//MATH/Fileshares/Vestas") # or 
setwd("~/aspeciale/R_data_local")

data = read.delim("//MATH/Fileshares/Vestas/Extract_0001.csv", stringsAsFactors=FALSE)
data = read.delim("//MATH/Fileshares/Vestas/Extract_0001.csv")
data = read.delim("~/aspeciale/R_data_local/Extract_0001.csv")


#### define variables intro ####
timeStamp          = data$CMSTimeStamp
timeStamp_POSIXct  = as.POSIXct(timeStamp)
power              = data$PowerActual
generatorSpeed     = data$GeneratorSpeed

#### two variable ####
generatorSpeed_num = as.numeric(generatorSpeed)
rotorSpeed         = data$RotorSpeed 

#save(timeStamp_POSIXct, generatorSpeed_num, rotorSpeed,power, file = "dataSubsetNew.RData")

setwd("~/aspeciale/R_data")
load("datasubset.RData")

plot(rotorSpeed, generatorSpeed_num)
ratio = lm(rotorSpeed~generatorSpeed_num-1)
summary(ratio)

## Remove one negative value
rotorSpeed[rotorSpeed<0] = 0 # #1
plot(rotorSpeed, generatorSpeed_num)

rotorSpeed[rotorSpeed==0]

## Set all generato speeds equal to zero when rotorspeed is zero
findzero = function(data){
  v = 0
  for(i in 1:length(data)){
  if(data[i]==0){
    v=c(v,i)
  }
}
return(v)
}

rotorzero = findzero(data=rotorSpeed); rotorzero
generatorSpeed_num[rotorzero]
generatorSpeed_num[rotorzero]=0

plot(rotorSpeed, generatorSpeed_num, pch = ".") # now two outliers and all NA are set to (0,0)


ratio = lm(rotorSpeed~generatorSpeed_num-1)
summary(ratio)

count_NA_rotor     = sum(is.na(rotorSpeed)); count_NA_rotor             # zero
count_NA_generator = sum(is.na(generatorSpeed_num)); count_NA_generator # 94. 10 after above is run


findNA = function(data){
  v = 0
  for(i in 1:length(data)){
    if(is.na(data[i])){
      v=c(v,i)
    }
    
  }
  return(v)
}

generatorNA = findNA(generatorSpeed_num)
generatorSpeed_num[generatorNA] # yep 10 NA found
generatorSpeed_num[generatorNA]=0 # the 10 NA set to zero

#### fft of rotor and generator ####

par(mfrow=c(1,2))
Sx_rotor = abs(fft(rotorSpeed))^2
plot(Sx_rotor, xlim = c(0,19500), ylim = c(0,1000), type = "l")

Sx_generator = abs(fft(generatorSpeed_num))^2
plot(Sx_generator, ylim = c(0,.5e7), type = "l")

summary(generatorSpeed_num)

#### ts ####
library(astsa)
acf2(rotorSpeed,15) # looks like AR(11) since ACF tails of, and PACF cuts of after lag 11
acf2(rotorSpeed,1000)

acf2(generatorSpeed_num,15) # looks like AR(11) since ACF tails of, and PACF cuts of after lag 11
acf2(generatorSpeed_num,1000)

?acf
# ts_rotor = as.ts(rotorSpeed) # not neccesary
par(mfrow=c(2,2))
acf(rotorSpeed)
pacf(rotorSpeed)
acf(generatorSpeed_num)
pacf(generatorSpeed_num)
par(mfrow=c(1,1))

acf2(rotorSpeed,1000); acf2(generatorSpeed_num,1000)
library(tseries)
adf.test(rotorSpeed)         # looks stationary
adf.test(generatorSpeed_num) # looks stationary

#### test intro ####
typeof(timeStamp) # interger
class(timeStamp) # factor
length(power) # 19528

class(timeStamp_POSIXct)

plot(timeStamp_POSIXct, power, pch = ".", xlab = "time")
plot(timeStamp_POSIXct, generatorSpeed, pch = ".", xlab = "time")
plot(timeStamp_POSIXct, rotorSpeed, pch = ".", xlab = "time")

#### 100 points test timeStamp ####
timeStamp100 = data$CMSTimeStamp[1:100]
power100 = data$PowerActual[1:100]
generatorSpeed100 = data$GeneratorSpeed[1:100]
rotorSpeed100 = data$RotorSpeed[1:100]

plot(timeStamp100,generatorSpeed100)

typeof(timeStamp100[1])
class(timeStamp100)

as.numeric(timeStamp100)
class(timeStamp100)
?POSIXlt
?POSIXct
POSIXct_time100 = as.POSIXct(timeStamp100)
POSIXct_time100
class(POSIXct_time)
plot(POSIXct_time100,power100, pch = 20 )

timeStamp100[2]-timeStamp100[1]

timeStamp5 = data$CMSTimeStamp[1:5]
power5 = data$PowerActual[1:5]
plot(timeStamp5,power5)

#### lm of wr and wr ####

m_wrt = lm(rotorSpeed[3:20]~rotorSpeed[2:19])

plot(rotorSpeed[3:20],rotorSpeed[2:19])
abline(m_wrt)
summary(m_wrt)

wr_minus_wg  = rotorSpeed-generatorSpeed_num # omega_{r,t-2}-omega_{g,t-2}

#### w_{r,t} = a * w_{r,t-1} + b * w_{g,t-1} + c * (w_{r,t-2}-w_{g,t-2})
m_wrt = lm(rotorSpeed[3:20]~rotorSpeed[2:19]+generatorSpeed_num[2:19]+wr_minus_wg[1:18])
m_wrt
plot(m_wrt)
summary(m_wrt)
plot(rotorSpeed[3:20],m_wrt$residuals)
abline(a=0,b=0)
qqplot(rotorSpeed[3:20],m_wrt$residuals)

m_wgt = lm(generatorSpeed_num[3:20]~rotorSpeed[2:19]+generatorSpeed_num[2:19]+wr_minus_wg[1:18])
m_wgt
summary(m_wgt)
plot(m_wgt)

coefficient_wrt = m_wrt$coefficients; coefficient_wrt
coefficient_wgt = m_wgt$coefficients; coefficient_wgt

#### w_{r,t} = a * w_{r,t-1} + b * w_{g,t-1}
m_wrt_udenQ = lm(rotorSpeed[2:19528]~rotorSpeed[1:19527]+generatorSpeed_num[1:19527]-1)
m_wgt_udenQ = lm(generatorSpeed_num[2:19528]~rotorSpeed[1:19527]+generatorSpeed_num[1:19527]-1)

coefficient_wrt_udenQ = m_wrt_udenQ$coefficients; coefficient_wrt_udenQ
coefficient_wgt_udenQ = m_wgt_udenQ$coefficients; coefficient_wgt_udenQ


#### parameter estimation - constant DLM ####

library(dlm)

A11=1;A21=1;A31=1;A12=1;A22=1;A32=1;A13=1;A23=1;A33=1
T = 120

Ac = matrix(c(A11,A21,A31,A12,A22,A32,A13,A23,A33),nrow = 3);Ac
A = diag(3)+Ac*T; A

m0 = c(0,0,0)
C0 = diag(3)
GG = A                                # matrix I+delta*T*A
FF = matrix(c(1,0,0,1,0,0), nrow = 2) # matrix C
V  = diag(2)
W  = diag(3)

dlmmodel = dlm(m0=m0, C0=C0, FF=FF, V=V, GG=GG, W=W ); dlmmodel
str(dlmmodel)
dlmfilter = dlmFilter(c(rotorSpeed,generatorSpeed),dlmmodel,simplify = TRUE)
summary(dlmfilter)

predictionError = y-esty

optim(c(),predictionError)