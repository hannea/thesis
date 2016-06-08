#=========================================================================================
# Description: load and save 4WTs_1Year_1Failure using sqldf package
#
# Load: CSV file
# Save: as RData file
#=========================================================================================

setwd("~/aspeciale/R_data_local")
# data = read.csv("~/aspeciale/R_data_local/Extract0005.csv", stringsAsFactors=FALSE)

# or
library("ff")
x = read.csv.ffdf(file="~/aspeciale/R_data_local/Extract0005.csv", header=TRUE, 
                  VERBOSE=TRUE, first.rows=10000, 
                  next.rows=50, colClasses=NA)

t1 = Sys.time()
save(data, file = "data_failure.RData")
t2 = Sys.time()
time = t2-t1

# or
library(sqldf)
f = file("~/aspeciale/R_data_local/Extract0005.csv")
t1 = Sys.time()
data = sqldf("select * from f", dbname = tempfile(), 
                           file.format = list(header = T, row.names = F))
t2 = Sys.time()
time=t2-t1
time # 5.49 mins

save(data, file = "4WTs_1Year_1Failure.RData")

t1 = Sys.time()
load("4WTs_1Year_1Failure.RData")
t2 = Sys.time()
time = t2-t1
time # 24.08 secs
