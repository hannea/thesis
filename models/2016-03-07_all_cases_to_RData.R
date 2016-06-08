#=========================================================================================
# Description: Convert case database fiels to RData from csv to decrease loading time
#
# Load: all cases as CSV files
# Save: all cases as RData files
#=========================================================================================

setwd("//math.aau.dk/Fileshares/Vestas")
setwd("~/aspeciale/R_data_local/all_cases")


for (i in 0:178){
  ## read baseline file (training)
  bl_file_name = paste('~/aspeciale/R_data_local/all_cases/',i,'_bl.csv', sep = "")
  bl = read.csv(bl_file_name, stringsAsFactors = FALSE)
  bl_file_name_RData = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_bl.RData', sep = "")
  save(bl, file = bl_file_name_RData)
  
  ## read test file
  m_file_name = paste('~/aspeciale/R_data_local/all_cases/',i,'_m.csv', sep = "")
  m = read.csv(m_file_name, stringsAsFactors = FALSE)
  m_file_name_RData = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_m.RData', sep = "")
  save(m, file = m_file_name_RData)
  
  ## read meta file
  meta_file_name = paste('~/aspeciale/R_data_local/all_cases/',i,'_meta.csv', sep = "")
  meta = read.csv(meta_file_name, stringsAsFactors = FALSE)
  meta_file_name_RData = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_meta.RData', sep = "")
  save(meta, file = meta_file_name_RData)
}



bl = read.csv("~/aspeciale/R_data_local/all_cases/0_bl.csv", stringsAsFactors = FALSE)
m  = read.csv("~/aspeciale/R_data_local/all_cases/0_m.csv", stringsAsFactors = FALSE)
meta = read.csv("~/aspeciale/R_data_local/all_cases/0_meta.csv", stringsAsFactors = FALSE)


# save all meta data in vector ------------------------------------------------------
meta_df = NULL
for (i in 0:178){
  meta_file_name = paste('~/aspeciale/R_data_local/all_cases_RData/',i,'_meta.RData', sep = "")
  load(meta_file_name)
  
  # meta[[2]] = as.POSIXct(meta[[2]], tz = "GMT") # do not save as POSIX since when loaded R changes it
  meta_df = rbind(meta_df, meta)
}

meta_df = tbl_df(meta_df)

#setwd("~/aspeciale/R_data_local/all_cases_RData")
#setwd("//math/Fileshares/Vestas/cases/all_cases_RData")
#save(meta_df, file = "meta_tot_df.RData")

#save(meta_u174, file = "meta_u174_df.RData")
