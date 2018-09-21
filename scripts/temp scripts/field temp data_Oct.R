# temp loggers pan KBay
# across depths and sites

#clear work space
rm(list=ls())
ls()

main<-setwd(getwd())
setwd("~/Desktop/Research and Teaching/UH MANOA/Research/Isotopes pan KBay/R")

library(reshape2)
library(plyr)
library(lubridate)
library(zoo)

# remove all unwanted rows and columns
# split date and time column

# Temp1_Oct2016: SN 10487936
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Temp1_Oct2016_10487936.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*0.9994+0.09144)

head(df)
temp1_10487936<-df
#####################

# Temp2_Oct_2016: SN 10779057
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Temp2_Oct2016_10779057.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.041-0.9401)

head(df)
temp2_10779057<-df
#####################

# Temp3_Oct_2016: SN 10779058
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Temp3_Oct2016_10779058.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.036-0.8501)

head(df)
temp3_10779058<-df
#####################


# Temp4_Oct_2016: SN 10779068
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Temp4_Oct2016_10779068.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.027-0.7018)

head(df)
temp4_10779068<-df
#####################

# Temp5_Oct_2016: SN 10487930
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Temp5_Oct2016_10487930.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.048-1.105)

head(df)
temp5_10487930<-df
#####################

# Temp6_Oct_2016: SN 10487935
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Temp6_Oct2016_10487935.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.028-0.6453)

head(df)
temp6_10487935<-df
#####################

# Temp7_Oct_2016: SN 10779066
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Temp7_Oct2016_10779066.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.023-0.5129)

head(df)
temp7_10779066<-df
#####################

# Temp8_Oct2016: SN 10635916 
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Temp8_Oct2016_10635916.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.041-1.129)

head(df)
temp8_10635916<-df
#####################

# logger 9: Rf44_Oct3_10209515 
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Rf44_Oct3_10209515.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*0.9762+0.5346)

head(df)
temp9_10209515 <-df
#####################

# logger 10: Rf42_Oct3_9768617
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Rf42_Oct3_9768617.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.01-0.4314)

head(df)
temp10_9768617<-df
#####################

# logger 11: Rf25_Oct3_9768603.csv
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Rf25_Oct3_9768603.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*0.9542+0.8276)

head(df)
temp11_9768603<-df
#####################

# logger 12: RfHIMB_Oct3_9768614.csv
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/RfHIMB_Oct3_9768614.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*0.7982+4.703)

head(df)
temp12_9768614<-df

#####################


#######################
# make one large dataframe with date, time, and calibration mean for all loggers
ALL.temps<-cbind(temp1_10487936[c(1,3)], temp2_10779057[c(0,3)], temp3_10779058[c(0,3)], temp4_10779068[c(0,3)], temp5_10487930[c(0,3)], temp6_10487935[c(0,3)], temp7_10779066[c(0,3)], temp8_10635916[c(0,3)], temp9_10209515[c(0,3)], temp10_9768617[c(0,3)], temp11_9768603[c(0,3)], temp12_9768614[c(0,3)])

colnames(ALL.temps)<-c("timestamp", "temp1_10487936_CalTemp", "temp2_10779057_CalTemp", "temp3_10779058_CalTemp", "temp4_10779068_CalTemp", "temp5_10487930_CalTemp", "temp6_10487935_CalTemp", "temp7_10779066_CalTemp", "temp8_10635916_CalTemp", "temp9_10209515_CalTemp", "temp10_9768617_CalTemp", "temp11_9768603_CalTemp", "temp12_9768614_CalTemp")

write.csv(ALL.temps,"Field.alltemps.Oct2016.csv")

###############
# figures
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
plot(ALL.temps$temp9_10209515_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp10_9768617_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp11_9768603_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp12_9768614_CalTemp~ALL.temps$timestamp)


#########################
#########################
# organize dataframe "ALL.temps" to relect site and depth
colnames(ALL.temps)
colnames(ALL.temps)<-c("timestamp", "Rf44.shallow", "Rf44.deep",
                       "Rf42.deep", "Rf42.shallow", "Rf25.shallow", 
                       "Rf25.deep", "HIMB.shallow", "HIMB.deep", 
                       "Rf44.mid", "Rf42.mid", "Rf25.mid", 
                       "HIMB.mid")

ALL.temps<-ALL.temps[, c(1:2,10,3,5,11,4,6,12,7,8,13,9)] #reorder columns from N>S and shallow > deep
head(ALL.temps)

df<-ALL.temps # rename dataframe
df$timestamp<-as.Date(df$timestamp) # change to DATE ONLY format to calulate range, means
df.split <- split(df, f=df$timestamp
                    < as.Date("2014-10-10", format="%F")) # split df by date

head(df)
# calculate daily means, aggregate into one dataframe
df.mean <- aggregate(data.frame(Rf44.shall.mean=df.split[[1]]$Rf44.shallow,
                                Rf44.mid.mean=df.split[[1]]$Rf44.mid,
                                Rf44.deep.mean=df.split[[1]]$Rf44.deep,
                                Rf42.shall.mean=df.split[[1]]$Rf42.shallow,
                                Rf42.mid.mean=df.split[[1]]$Rf42.mid,
                                Rf42.deep.mean=df.split[[1]]$Rf42.deep,
                                Rf25.shall.mean=df.split[[1]]$Rf25.shallow,
                                Rf25.mid.mean=df.split[[1]]$Rf25.mid,
                                Rf25.deep.mean=df.split[[1]]$Rf25.deep, 
                                HIMB.shall.mean=df.split[[1]]$HIMB.shallow,
                                HIMB.mid.mean=df.split[[1]]$HIMB.mid, 
                                HIMB.deep.mean=df.split[[1]]$HIMB.deep) ,
                     by=list(Date=df.split[[1]]$timestamp), FUN=mean)

# calculate daily maximum, aggregate into one dataframe
df.max <- aggregate(data.frame(Rf44.shall.max=df.split[[1]]$Rf44.shallow,
                               Rf44.mid.max=df.split[[1]]$Rf44.mid,
                               Rf44.deep.max=df.split[[1]]$Rf44.deep,
                               Rf42.shall.max=df.split[[1]]$Rf42.shallow,
                               Rf42.mid.max=df.split[[1]]$Rf42.mid,
                               Rf42.deep.max=df.split[[1]]$Rf42.deep,
                               Rf25.shall.max=df.split[[1]]$Rf25.shallow,
                               Rf25.mid.max=df.split[[1]]$Rf25.mid,
                               Rf25.deep.max=df.split[[1]]$Rf25.deep, 
                               HIMB.shall.max=df.split[[1]]$HIMB.shallow,
                               HIMB.mid.max=df.split[[1]]$HIMB.mid, 
                               HIMB.deep.max=df.split[[1]]$HIMB.deep) ,
                    by=list(Date=df.split[[1]]$timestamp), FUN=max)

# calculate daily minumum, aggregate into one dataframe
df.min <- aggregate(data.frame(Rf44.shall.min=df.split[[1]]$Rf44.shallow,
                               Rf44.mid.min=df.split[[1]]$Rf44.mid,
                               Rf44.deep.min=df.split[[1]]$Rf44.deep,
                               Rf42.shall.min=df.split[[1]]$Rf42.shallow,
                               Rf42.mid.min=df.split[[1]]$Rf42.mid,
                               Rf42.deep.min=df.split[[1]]$Rf42.deep,
                               Rf25.shall.min=df.split[[1]]$Rf25.shallow,
                               Rf25.mid.min=df.split[[1]]$Rf25.mid,
                               Rf25.deep.min=df.split[[1]]$Rf25.deep, 
                               HIMB.shall.min=df.split[[1]]$HIMB.shallow,
                               HIMB.mid.min=df.split[[1]]$HIMB.mid, 
                               HIMB.deep.min=df.split[[1]]$HIMB.deep) ,
                    by=list(Date=df.split[[1]]$timestamp), FUN=min)

# calculate daily range, aggregate into one dataframe
df.range <- data.frame(df.max[[1]], 
                       Rf44.shall.range=df.max$Rf44.shall.max-df.min$Rf44.shall.min,
                       Rf44.mid.range= df.max$Rf44.mid.max-df.min$Rf44.mid.min,
                       Rf44.deep.range=df.max$Rf44.deep.max-df.min$Rf44.deep.min,
                       Rf42.shall.range=df.max$Rf42.shall.max-df.min$Rf42.shall.min,
                       Rf42.mid.range=df.max$Rf42.mid.max-df.min$Rf42.mid.min,
                       Rf42.deep.range=df.max$Rf42.deep.max-df.min$Rf42.deep.min,
                       Rf25.shall.range=df.max$Rf25.shall.max-df.min$Rf25.shall.min,
                       Rf25.mid.range=df.max$Rf25.mid.max-df.min$Rf25.mid.min, 
                       Rf25.deep.range=df.max$Rf25.deep.max-df.min$Rf25.deep.min, 
                       HIMB.shall.range=df.max$HIMB.shall.max-df.min$HIMB.shall.min,
                       HIMB.mid.range=df.max$HIMB.mid.max-df.min$HIMB.mid.min,
                       HIMB.deep.range=df.max$HIMB.deep.max-df.min$HIMB.deep.min)
write.csv(df.range, "Oct.temp range.csv")

##############################
# make dataframe for each reef

# Reef 44
Rf44.temp.Oct<-data.frame(df.mean[, c(1:4)], df.max[, c(2:4)], df.min[, c(2:4)], df.range[, c(2:4)])

# Reef 42
Rf42.temp.Oct<-data.frame(df.mean[, c(1,5:7)], df.max[, c(5:7)], df.min[, c(5:7)], df.range[, c(5:7)])

# Reef 25
Rf25.temp.Oct<-data.frame(df.mean[, c(1,8:10)], df.max[, c(8:10)], df.min[, c(8:10)], df.range[, c(8:10)])

# HIMB
HIMB.temp.Oct<-data.frame(df.mean[, c(1,11:13)], df.max[, c(11:13)], df.min[, c(11:13)], df.range[, c(11:13)])


#########################
# figures
#########################

pdf(paste("Oct.temps",".pdf", sep=""))

# Reef 44
reefcols <- c("#6CA6CD", "#2E8B57", "#8B5742")
par(mfrow=c(2,2), mar=c(2,4,1,1), mgp=c(2,0.5,0))

# Timespan
# the mean here is a dummy variable, need the "date" and "mean" for the figures
Timespan.df<-data.frame(df.mean[, (1:2)]); colnames(Timespan.df)<-c("Date", "mean")

k=1; lwd=1 # k-day moving averages
plot(mean ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 44 Temp mean")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf44.temp.Oct$Date, mean=rollmean(Rf44.temp.Oct$Rf44.shall.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Oct$Date, mean=rollmean(Rf44.temp.Oct$Rf44.mid.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Oct$Date, mean=rollmean(Rf44.temp.Oct$Rf44.deep.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[3], lwd=2)
})

#########################
# Reef 42
plot(mean ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 42 Temp mean")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf42.temp.Oct$Date, mean=rollmean(Rf42.temp.Oct$Rf42.shall.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf42.temp.Oct$Date, mean=rollmean(Rf42.temp.Oct$Rf42.mid.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf42.temp.Oct$Date, mean=rollmean(Rf42.temp.Oct$Rf42.deep.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[3], lwd=2)
})

#########################
# Reef 25
plot(mean ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 25 Temp mean")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf25.temp.Oct$Date, mean=rollmean(Rf25.temp.Oct$Rf25.shall.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf25.temp.Oct$Date, mean=rollmean(Rf25.temp.Oct$Rf25.mid.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf25.temp.Oct$Date, mean=rollmean(Rf25.temp.Oct$Rf25.deep.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[3], lwd=2)
})

#########################
# Reef HIMB
plot(mean ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="HIMB Temp mean")
mtext(expression(bold("D")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=HIMB.temp.Oct$Date, mean=rollmean(HIMB.temp.Oct$HIMB.shall.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=HIMB.temp.Oct$Date, mean=rollmean(HIMB.temp.Oct$HIMB.mid.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=HIMB.temp.Oct$Date, mean=rollmean(HIMB.temp.Oct$HIMB.deep.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[3], lwd=2)
})






# MAXIMUM TEMPERATURES
#########################

# Reef 44
colnames(Timespan.df)<-c("Date", "max") # change to 'max' to match dataframe here
plot(max ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 44 Temp max")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf44.temp.Oct$Date, max=rollmean(Rf44.temp.Oct$Rf44.shall.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Oct$Date, max=rollmean(Rf44.temp.Oct$Rf44.mid.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Oct$Date, max=rollmean(Rf44.temp.Oct$Rf44.deep.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[3], lwd=2)
})

#########################
# Reef 42
plot(max ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 42 Temp max")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf42.temp.Oct$Date, max=rollmean(Rf42.temp.Oct$Rf42.shall.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf42.temp.Oct$Date, max=rollmean(Rf42.temp.Oct$Rf42.mid.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf42.temp.Oct$Date, max=rollmean(Rf42.temp.Oct$Rf42.deep.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[3], lwd=2)
})

#########################
# Reef 25
plot(max ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef25 Temp max")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf25.temp.Oct$Date, max=rollmean(Rf25.temp.Oct$Rf25.shall.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf25.temp.Oct$Date, max=rollmean(Rf25.temp.Oct$Rf25.mid.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf25.temp.Oct$Date, max=rollmean(Rf25.temp.Oct$Rf25.deep.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[3], lwd=2)
})

#########################
# Reef HIMB
plot(max ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="HIMB Temp max")
mtext(expression(bold("D")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=HIMB.temp.Oct$Date, max=rollmean(HIMB.temp.Oct$HIMB.shall.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=HIMB.temp.Oct$Date, max=rollmean(HIMB.temp.Oct$HIMB.mid.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=HIMB.temp.Oct$Date, max=rollmean(HIMB.temp.Oct$HIMB.deep.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[3], lwd=2)
})




# MINIMUM TEMPERATURES
#########################

# Reef 44
colnames(Timespan.df)<-c("Date", "min") # change to 'max' to match dataframe here
plot(min ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 44 Temp min")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf44.temp.Oct$Date, min=rollmean(Rf44.temp.Oct$Rf44.shall.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Oct$Date, min=rollmean(Rf44.temp.Oct$Rf44.mid.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Oct$Date, min=rollmean(Rf44.temp.Oct$Rf44.deep.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[3], lwd=2)
})

#########################
# Reef 42
plot(min ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 42 Temp min")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf42.temp.Oct$Date, min=rollmean(Rf42.temp.Oct$Rf42.shall.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=2) 
})

with(na.omit(data.frame(date=Rf42.temp.Oct$Date, min=rollmean(Rf42.temp.Oct$Rf42.mid.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf42.temp.Oct$Date, min=rollmean(Rf42.temp.Oct$Rf42.deep.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[3], lwd=2)
})

#########################
# Reef 25
plot(min ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 25 Temp min")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf25.temp.Oct$Date, min=rollmean(Rf25.temp.Oct$Rf25.shall.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf25.temp.Oct$Date, min=rollmean(Rf25.temp.Oct$Rf25.mid.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf25.temp.Oct$Date, min=rollmean(Rf25.temp.Oct$Rf25.deep.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[3], lwd=2)
})

#########################
# Reef HIMB
plot(min ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="", main="HIMB Temp min")
mtext(expression(bold("D")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=HIMB.temp.Oct$Date, min=rollmean(HIMB.temp.Oct$HIMB.shall.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=HIMB.temp.Oct$Date, min=rollmean(HIMB.temp.Oct$HIMB.mid.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=HIMB.temp.Oct$Date, min=rollmean(HIMB.temp.Oct$HIMB.deep.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[3], lwd=2)
})





# RANGE TEMPERATURES
#########################

# Reef 44
colnames(Timespan.df)<-c("Date", "range")
plot(range ~ Date, Timespan.df, type="n", ylab=Delta~"Temperature (°C)"~day^-1, ylim=c(0, 4), xaxt="n", xlab="", main="Reef 44 Temp range")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf44.temp.Oct$Date, range=rollmean(Rf44.temp.Oct$Rf44.shall.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Oct$Date, range=rollmean(Rf44.temp.Oct$Rf44.mid.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Oct$Date, range=rollmean(Rf44.temp.Oct$Rf44.deep.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[3], lwd=2)
})

#########################
# Reef 42
plot(range ~ Date, Timespan.df, type="n", ylab=Delta~"Temperature (°C)"~day^-1, ylim=c(0,4), xaxt="n", xlab="", main="Reef 42 Temp range")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf42.temp.Oct$Date, range=rollmean(Rf42.temp.Oct$Rf42.shall.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf42.temp.Oct$Date, range=rollmean(Rf42.temp.Oct$Rf42.mid.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf42.temp.Oct$Date, range=rollmean(Rf42.temp.Oct$Rf42.deep.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[3], lwd=2)
})

#########################
# Reef 25
plot(range ~ Date, Timespan.df, type="n", ylab=Delta~"Temperature (°C)"~day^-1, ylim=c(0, 4), xaxt="n", xlab="",  main="Reef 25 Temp range")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf25.temp.Oct$Date, range=rollmean(Rf25.temp.Oct$Rf25.shall.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf25.temp.Oct$Date, range=rollmean(Rf25.temp.Oct$Rf25.mid.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf25.temp.Oct$Date, range=rollmean(Rf25.temp.Oct$Rf25.deep.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[3], lwd=2)
})

#########################
# Reef HIMB
plot(range ~ Date, Timespan.df, type="n", ylab=Delta~"Temperature (°C)"~day^-1, ylim=c(0, 4), xaxt="n", xlab="",  main="HIMB Temp range")
mtext(expression(bold("D")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=HIMB.temp.Oct$Date, range=rollmean(HIMB.temp.Oct$HIMB.shall.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=HIMB.temp.Oct$Date, range=rollmean(HIMB.temp.Oct$HIMB.mid.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=HIMB.temp.Oct$Date, range=rollmean(HIMB.temp.Oct$HIMB.deep.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[3], lwd=2)
})

dev.off()






###########################

df # all data dataframe
head(df)

# dummy 2-column dataframe to give the Days for all columns and a "mean" to graph
All.dates<-df[, c(1:2)]; colnames(All.dates)<-c("Date", "Caltemp") # with date and time
All.dates$Date<-as.Date(All.dates$Date) # format as Date, this will be x axis for all figures
head(All.dates)

reefcols <- c("#6CA6CD", "#2E8B57", "#8B5742")
par(mfrow=c(2,2), mar=c(2,4,1,1), mgp=c(2,0.5,0))
k=1; lwd=1 # k-day moving averages

plot(Caltemp ~ Date, All.dates, type="n", ylab=Delta~"Temperature (°C)"~d^-1, ylim=c(24, 30), xaxt="n", xlab="")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(All.dates$Date), max(All.dates$Date), by="1 d"), format="%m/%d")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=All.dates$Date, Caltemp=df$HIMB.shallow, fill=NA)), { 
  lines(date, mean, col=reefcols[1], lwd=2) 
})




with(na.omit(data.frame(date=df$timestamp, Caltemp=rollmean(df$HIMB.mid, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=df$timestamp, Caltemp=rollmean(df$HIMB.deep, k, fill=NA))), { 
  lines(date, mean, col=reefcols[3], lwd=2)
})



## need calibration files




# logger 11: Rf25_Oct3_9768603

# note that this logger stops at 10/28/16, others go until 11/3/16
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Rf25_Oct3_9768603.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-10-27 23:45:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*0.9762+0.5346) ## need calibrtion here

head(df)
temp11_10209515<-df
#####################
