# temp loggers pan KBay
# across depths and sites

#clear work space
rm(list=ls())
ls()

main<-setwd(getwd())
setwd("~/Desktop/Research and Teaching/UH MANOA/Research/Isotopes pan KBay/R")

library(reshape2)
library(plyr)
library(ggplot2)

# remove all unwanted rows and columns
# split date and time column

# Temp1_Nov2016: SN 10487936
#####################
data<-read.csv("data/cross depth PAR temp/Temp1_Nov2016_10487936.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-11-03 11:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*0.9994+0.09144)

head(df)
temp1_10487936<-df
#####################

# Temp2_Nov_2016: SN 10779057
#####################
data<-read.csv("data/cross depth PAR temp/Temp2_Nov2016_10779057.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-11-03 11:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.041-0.9401)

head(df)
temp2_10779057<-df
#####################

# Temp3_Nov_2016: SN 10779058
#####################
data<-read.csv("data/cross depth PAR temp/Temp3_Nov2016_10779058.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-11-03 11:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.036-0.8501)

head(df)
temp3_10779058<-df
#####################


# Temp4_Nov_2016: SN 10779068
#####################
data<-read.csv("data/cross depth PAR temp/Temp4_Nov2016_10779068.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-11-03 11:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.027-0.7018)

head(df)
temp4_10779068<-df
#####################

# Temp5_Nov_2016: SN 10487930
#####################
data<-read.csv("data/cross depth PAR temp/Temp5_Nov2016_10487930.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-11-03 11:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.048-1.105)

head(df)
temp5_10487930<-df
#####################

# Temp6_Nov_2016: SN 10487935
#####################
data<-read.csv("data/cross depth PAR temp/Temp6_Nov2016_10487935.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-11-03 11:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.028-0.6453)

head(df)
temp6_10487935<-df
#####################

# Temp7_Nov_2016: SN 10779066
#####################
data<-read.csv("data/cross depth PAR temp/Temp7_Nov2016_10779066.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-11-03 11:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.023-0.5129)

head(df)
temp7_10779066<-df
#####################

# Temp8_Nov2016: SN 10635916 
#####################
data<-read.csv("data/cross depth PAR temp/Temp8_Nov2016_10635916.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-11-03 11:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.041-1.129)

head(df)
temp8_10635916<-df
#####################


#######################
# make one large dataframe with date, time, and calibration mean for all loggers
ALL.temps<-cbind(temp1_10487936[c(1,3)], temp2_10779057[c(0,3)], temp3_10779058[c(0,3)], temp4_10779068[c(0,3)], temp5_10487930[c(0,3)], temp6_10487935[c(0,3)], temp7_10779066[c(0,3)], temp8_10635916[c(0,3)])

colnames(ALL.temps)<-c("timestamp", "temp1_10487936_CalTemp", "temp2_10779057_CalTemp", "temp3_10779058_CalTemp", "temp4_10779068_CalTemp", "temp5_10487930_CalTemp", "temp6_10487935_CalTemp", "temp7_10779066_CalTemp", "temp8_10635916_CalTemp")
ALL.temps$timestamp<-as.POSIXct(ALL.temps$timestamp) # need to do this for graphing data

par(mfrow=c(1,1))
plot(ALL.temps$temp1_10487936_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp2_10779057_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp3_10779058_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp4_10779068_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp5_10487930_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp6_10487935_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp7_10779066_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp8_10635916_CalTemp~ALL.temps$timestamp)
