#=========================================================================================
# Description: Test if any loads are other than "GeneratorSpeed" and 
# "GeR120RRefSpeedNoFilActual". Furtermore generate data frames with sensor locations and
# vibration signals.
#
# Load: all cases
# Save: save data frames to join to the monitor output
#=========================================================================================

## load signal
for (i in 0:178){
  print(i)
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  load(bl_file_name)
  if (names(bl[3]) != "GeneratorSpeed" & names(bl[3]) != "GeR120RRefSpeedNoFilActual") {
    print(names(bl[3]))
  }
}

## print all vibration signals
for (i in 0:178){
  print(i)
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  load(bl_file_name)
  print(names(bl[2]))
}

## Test sensor names in vib (bl[2]) and create sensor_df and save

sensor_df = NULL
for (i in 0:178){
  print(i)
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  load(bl_file_name)
  print(names(bl[2]))
  if (grepl("Generator", names(bl[2])) | grepl("GeNR", names(bl[2])) | grepl("GeR", names(bl[2]))) {
    sensor = "generator"
  } else if (grepl("High", names(bl[2])) | grepl("Intermediate", names(bl[2])) | 
             grepl("Gb", names(bl[2])) | grepl("Pl1", names(bl[2]))) {
    sensor = "gearbox"
  } else if (grepl("MainBear", names(bl[2]))) {
    sensor = "main_bearing"
  } else {
    sensor = "? unknown ?" # none
  }
  sensor_df = rbind(sensor_df, data.frame(index = i, vib_name = names(bl[2]), sensor))
}

setwd("~/aspeciale/R_data_local/all_cases_RData")
#save(sensor_df, file = "sensor_names.RData")

## print only gearbox labels (not generator and not main bearing)
for (i in 0:178){
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  load(bl_file_name)
  if (grepl("Generator", names(bl[2])) | grepl("GeNR", names(bl[2])) | grepl("GeR", names(bl[2])) |
      grepl("MainBear", names(bl[2]))) {next}
  print(i)
  print(names(bl[2]))
}


## Create signal_df and save

signal_df = NULL
for (i in 0:178){
  print(i)
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  load(bl_file_name)
  print(names(bl[2]))
  if (grepl("RMS", names(bl[2]))) {
    signal = "RMS"
  } else if (grepl("ECU", names(bl[2]))) {
    signal = "ECU"
  } else {
    signal = "other"
  }
  signal_df = rbind(signal_df, data.frame(index = i, vib_name = names(bl[2]), signal))
}
summary(signal_df$signal)
setwd("~/aspeciale/R_data_local/all_cases_RData")
#save(signal_df, file = "signal_RMS_ECU_names.RData")
