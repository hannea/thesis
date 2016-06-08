#=========================================================================================
# Description: plot of different variables, such as: generator, amplitude, RMS, phase etc.
#
# Load: "4WT_1Year_1Failure.RData", or "4WT_1Month.Rdata"
#=========================================================================================

setwd("~/aspeciale/R_data_local")
load("4WT_1Year_1Failure.RData") # something wrong with the times? Due to summer time!!!
load("4WT_1Month.Rdata")
setwd("C:/Users/Hanne/Dropbox/aspeciale/R/data_investigation/functions")
source("identify_WTs_func_Vestas_file.R")
identified_data = identify_WTs_func(data)
time1 = identified_data$WT1$CMSTimeStamp
int = 1:15000

library("ggplot2")
########################################################################
## GeneratorDE
########################################################################
generatorDE = grep("GeneratorDE",names(data)) 
length(generatorDE) # 9
par(mfrow = c(3,3), mar = c(4,4,1,1))
for(i in 1:length(generatorDE)){
  k = generatorDE[i]
  plot(time1[int], identified_data$WT1[[k]][int], xlab = "time", 
       ylab = names(identified_data$WT1[k]), pch = ".")
  #hist(identified_data$WT1[[k]][int], main = "")
}

for(i in 1:length(generatorDE)){
  k = generatorDE[i]
  plot(identified_data$WT1$PowerActual[int], identified_data$WT1[[k]][int], xlab = "Power", 
       ylab = names(identified_data$WT1[k]), pch = ".")
}

names(identified_data$WT1[generatorDE])

par(mfrow = c(1,1))
plot(identified_data$WT1$HFBPGeneratorDE, identified_data$WT1$HFPeakGeneratorDE)
abline(c(0,2),col="red",lwd=3)

########################################################################
## Aplitudes
########################################################################
amp = grep("Amplitude",names(data)) 
length(amp) # 34
par(mfrow = c(4,3), mar = c(4,4,1,1))
for(i in 1:length(amp)){
  k = amp[i]
  plot(time1[int], identified_data$WT1[[k]][int], xlab = "time", 
       ylab = "Amplitude", pch = ".")
  #hist(identified_data$WT1[[k]][int], main = "")
}
names(identified_data$WT1[amp])

## qplot
par(mfrow = c(4,3), mar = c(4,4,1,1))
for(i in 1:length(amp)){
  k = amp[i]
  qplot(time1[int], identified_data$WT1[[amp]][int])
  #hist(identified_data$WT1[[k]][int], main = "")
}



########################################################################
## RMS
########################################################################
rms = grep("RMS",names(data)) 
length(rms) # 16
par(mfrow = c(4,4), mar = c(4,4,1,1))
for(i in 1:length(rms)){
  k = rms[i]
  plot(time1[int], identified_data$WT1[[k]][int], xlab = "time", ylab = "RMS", pch = ".")
}
names(identified_data$WT1[rms])

########################################################################
## Phase
########################################################################
phase = grep("Phase",names(data)) 
length(phase) # 11
par(mfrow = c(4,3), mar = c(4,4,1,1))
for(i in 1:length(phase)){
  k = phase[i]
  plot(time1[int], identified_data$WT1[[k]][int], xlab = "time", ylab = "Phase", pch = ".")
}
names(identified_data$WT1[phase])

par(mfrow = c(4,3), mar = c(4,4,1,1))
for(i in 1:length(phase)){
  k = phase[i]
  plot(identified_data$WT1$GeneratorSpeed[int], identified_data$WT1[[k]][int], 
       xlab = "generator speed", ylab = "Phase", pch = ".")
}
names(identified_data$WT1[phase])

########################################################################
## Other variables
########################################################################
length(names(data)) # 104 variables in total
                    # first 7 is from controller - not vibration signal
length(amp) + length(rms) + length(phase) # 61 is aplitude, RMS and phase
hfbp = grep("HFBP",names(data));length(hfbp) # 8
hfpeak = grep("HFPeak",names(data));length(hfpeak) # 8
hfcrest = grep("HFCrest",names(data));length(hfcrest) # 8
ecu = grep("ECU",names(data));length(ecu) # 6 ?? what is this. Sum to 98 so far
rvp = grep("RVP",names(data));length(rvp) # 2
rvhigh = grep("RVHigh",names(data));length(rvhigh) # 2
rvinter = grep("RVIntermediate",names(data));length(rvinter) # 2
# total = 104

########################################################################
## Compare amplitude, RMS, and phase of GeneratorDE
########################################################################
int = 1:10000
par(mfrow = c(4,1), mar = c(1,4,1,1))
k = amp[1]; names(identified_data$WT1[k])
plot(time1[int], identified_data$WT1[[k]][int], xlab = "time", ylab = "Amplitude", 
     pch = ".", main = "GeneratorDE")
k = rms[1]; names(identified_data$WT1[k])
plot(time1[int], identified_data$WT1[[k]][int], xlab = "time", ylab = "RMS", pch = ".")
k = phase[1]; names(identified_data$WT1[k])
plot(time1[int], identified_data$WT1[[k]][int], xlab = "time", ylab = "Phase", pch = ".")
plot(time1[int], identified_data$WT1$PowerActual[int], xlab = "time", ylab = "Power", pch = ".")

## instead of against time - suggested by Anton, small times look uniform.
plot(identified_data$WT1$GeneratorSpeed[int], identified_data$WT1[[k]][int], pch=".")

########################################################################
## plot
########################################################################
par(mfrow = c(1,1))
# dev.off()
plot(identified_data$WT1[c(2,phase[1:5])], pch = ".") # warning: really slow!!
names(identified_data$WT1[c(2,phase[1:5])])

plot(identified_data$WT1[rms[1:5]], pch = ".")
names(identified_data$WT1[rms[1:5]])

# plot of different vibration signals
plot(identified_data$WT1[7:11], pch = ".")
names(identified_data$WT1[7:11])

plot(identified_data$WT1[c(2,4:6)], pch = ".")
names(identified_data$WT1[c(2,4:6)])

plot(identified_data$WT1[c(2,6)], pch = ".") # generator speed vs time
########################################################################
## plot of vibrations of sensor on GeneratorDE
########################################################################
plot(identified_data$WT1[c(2,6,9,10,11)], pch = ".")
names(identified_data$WT1[c(2,6,9,10,11)])

names(identified_data$WT1[1:15])
## times
int = 1:length(time1) #1:10000 # 1:120000
par(mfrow = c(4,1), mar = c(4,4,1.7,1))
plot(time1[int], identified_data$WT1[[6]][int], xlab = "time", ylab = "Gen. Speed", 
     pch = ".", main = "GeneratorDE")
plot(time1[int], identified_data$WT1[[9]][int], xlab = "time", ylab = "RMS", pch = ".")
plot(time1[int], identified_data$WT1[[10]][int], xlab = "time", ylab = "Amplitude", pch = ".")
plot(time1[int], identified_data$WT1[[11]][int], xlab = "time", ylab = "Phase", pch = ".")
# plot(time1[int], identified_data$WT1$PowerActual[int], xlab = "time", ylab = "Power", pch = ".")

par(mfrow = c(3,1), mar = c(4,4,1.7,1))
plot(identified_data$WT1[[6]][int], identified_data$WT1[[9]][int], 
     xlab = "Generator Speed", ylab = "RMSISO", pch = ".", main = "GeneratorDE")
plot(identified_data$WT1[[6]][int], identified_data$WT1[[10]][int], 
     xlab = "Generator Speed", ylab = "Harmonic Amplitude", pch = ".")
plot(identified_data$WT1[[6]][int], identified_data$WT1[[11]][int], 
     xlab = "Generator Speed", ylab = "Harmonic Phase", pch = ".")

head(identified_data$WT1$CMSTimeStamp)

########################################################################
## ggplot of vibrations and loads
########################################################################
# run top lines and
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)

# Time series of vibration and generator speed --------------------------------------
WT1 = identified_data$WT1[c(2,6,9:11)]
#ggplot(WT1, aes(CMSTimeStamp, PowerActual)) + geom_point()

WT1_m = gather(WT1, variable, value, -CMSTimeStamp) #melt(WT1, "CMSTimeStamp")

ggplot(WT1_m, aes(CMSTimeStamp, value, colour = variable)) + 
  geom_line(data = subset(WT1_m, variable != "GeneratorSpeed")) +
  geom_line(data = subset(WT1_m, variable == "GeneratorSpeed"), colour="black") +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  theme(legend.position="none") +
  xlab("time") + ylab("")

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("vibs_vs_time.png", width = 7, height = 5)

# Compare loads ---------------------------------------------------------------------
WT1 = identified_data$WT1[c(4,6,9:11)] # gen speed and power
WT1_sub = WT1_tidyr[1:2000,]

library(tidyr)
WT1_m = gather(WT1, vibname, vib,-c(PowerActual, GeneratorSpeed))
# melt(WT1, c("PowerActual", "GeneratorSpeed")) 
# same as melt above except vibs are variables
WT1_tidyr = gather(WT1_m, loadname,  load, c(PowerActual, GeneratorSpeed))


ggplot(WT1_tidyr, aes(load, vib, colour = vibname)) + #, colour = vibname
  geom_point(size=0.1) + #alpha = 0.05, 
  facet_grid(vibname~loadname, scales = "free") +
  theme(legend.position = "none") + 
  ylab("") + xlab("")

setwd("C:/Users/Hanne/Dropbox/aspeciale/Report/graphics/plots")
#ggsave("vibs_vs_loads.png", width = 7, height = 6.5)
