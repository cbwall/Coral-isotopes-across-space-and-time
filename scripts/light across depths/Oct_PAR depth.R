# light loggers pan KBay
# across depths and sites OCTOBER 2016

#clear work space
rm(list=ls())
ls()

main<-setwd(getwd())
setwd("~/Desktop/Research and Teaching/UH MANOA/Research/Isotopes pan KBay/R")

# Light logger 1: SN 2484, Reef 44 <1m
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/2484_logger1_Oct2016.csv", skip=8)
df<-data[, c(-1,-5)] # removes "trash" columns
colnames(df)<-c("Date", "Time", "Raw.PAR")
df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df<-df[ , c(4,3)] # timestamp and Raw value only
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
df$Calibrated.PAR<-(df$Raw.PAR*0.07403) # from calibration file
plot(df$Calibrated.PAR)
log1_2484<-df

# Light logger 3: SN 6378 Reef 42, >7m
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/6378_logger3_Oct2016.csv", skip=8)
df<-data[, c(-1,-5)] # removes "trash" columns
colnames(df)<-c("Date", "Time", "Raw.PAR")
df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df<-df[ , c(4,3)] # timestamp and Raw value only
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
df$Calibrated.PAR<-(df$Raw.PAR*0.2022) # from calibration file
plot(df$Calibrated.PAR)
log3_6378<-df

# Light logger 4: SN 2488, Reef 42, <1m
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/2488_logger4_Oct2016.csv", skip=8)
df<-data[, c(-1,-5)] # removes "trash" columns
colnames(df)<-c("Date", "Time", "Raw.PAR")
df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df<-df[ , c(4,3)] # timestamp and Raw value only
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
df$Calibrated.PAR<-(df$Raw.PAR*0.07519) # from calibration file
plot(df$Calibrated.PAR)
log4_2488<-df

# Light logger 7: SN 4807, HIMB, <1m
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/4807_logger7_Oct2016.csv", skip=8)
df<-data[, c(-1,-5)] # removes "trash" columns
colnames(df)<-c("Date", "Time", "Raw.PAR")
df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df<-df[ , c(4,3)] # timestamp and Raw value only
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
df$Calibrated.PAR<-(df$Raw.PAR*0.03904) # from calibration file
plot(df$Calibrated.PAR)
log7_4807<-df

# Light logger 8: SN 4804, HIMB, >7m
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/4804_logger8_Oct2016.csv", skip=8)
df<-data[, c(-1,-5)] # removes "trash" columns
colnames(df)<-c("Date", "Time", "Raw.PAR")
df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df<-df[ , c(4,3)] # timestamp and Raw value only
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
df$Calibrated.PAR<-(df$Raw.PAR*0.05199) # from calibration file
plot(df$Calibrated.PAR)
log8_4804<-df

# Light logger Raph: SN 4322, Reef 42, 2m
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Rf42_Oct3_4322.CSV", skip=8)
df<-data[, c(-1,-5)] # removes "trash" columns
colnames(df)<-c("Date", "Time", "Raw.PAR")
df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df<-df[ , c(4,3)] # timestamp and Raw value only
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
df$Calibrated.PAR<-(df$Raw.PAR*0.09929) # from calibration file
plot(df$Calibrated.PAR)
log10_4322<-df

# Light logger Raph: SN 4375, Reef 44, 2m
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/Rf44_Oct3_4375.CSV", skip=8)
df<-data[, c(-1,-5)] # removes "trash" columns
colnames(df)<-c("Date", "Time", "Raw.PAR")
df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df<-df[ , c(4,3)] # timestamp and Raw value only
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
df$Calibrated.PAR<-(df$Raw.PAR*0.05762) # from calibration file
plot(df$Calibrated.PAR)
log11_4375<-df

# Light logger Raph: SN 4805, HIMB, 2m
#####################
data<-read.csv("data/cross depth PAR temp/Oct 2016/RfHIMB_Oct3_4805.CSV", skip=8)
df<-data[, c(-1,-5)] # removes "trash" columns
colnames(df)<-c("Date", "Time", "Raw.PAR")
df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df<-df[ , c(4,3)] # timestamp and Raw value only
df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
df$Calibrated.PAR<-(df$Raw.PAR*0.07131) # from calibration file
plot(df$Calibrated.PAR)
log12_4805<-df

## combine dataframes
ALL.PAR<-cbind(log1_2484[c(1,3)], log3_6378[c(0,3)], log4_2488[c(0,3)], log7_4807[c(0,3)], log8_4804[c(0,3)],  log10_4322[c(0,3)], log11_4375[c(0,3)], log12_4805[c(0,3)])

colnames(ALL.PAR)<-c("timestamp", "log1_2484_CalPAR","log3_6378_CalPAR", "log4_2488_CalPAR", "log7_4807_CalPAR", "log8_4804_CalPAR", "log10_4322_CalPAR", "log11_4375_CalPAR", "log12_4805_CalPAR")

# write.csv(ALL.PAR,"Field.allPAR.Oct2016.csv")

#########################
#########################
# organize dataframe "ALL.temps" to relect site and depth
colnames(ALL.PAR)
colnames(ALL.PAR)<-c("timestamp", "Rf44.shallow", "Rf42.deep", "Rf42.shallow", "HIMB.shallow", "HIMB.deep", "Rf42.mid", "Rf44.mid", "HIMB.mid")

# reorder columns from N>S and shallow to deep
ALL.PAR<-ALL.PAR[, c(1:2,8,4,7,3,5,9,6)]
colnames(ALL.PAR)

############################
# calculate daily integrated light values for each logger
# multiply calibrated values (umol photons m-2 s-1) by 0.0864 to reach mol photons m-2 d-1
# umol/s.. /1,000,000 * 24h * 60m * 60s = 0.0864... same as if you integrated over 12h
# umol/s.. /1,000,000 * 12h * 60m * 60s = 0.0432...
# since averaging over dark period here, best way is to get DLI across 24h period 

df<-ALL.PAR
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp) # change to DATE ONLY format to calulate range, means
str(df)

df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-07", format="%F")) # split df by date

df.dli<-aggregate(data.frame(Rf44.shall.DLI=df.split[[1]]$Rf44.shallow*0.0864,
                         Rf44.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                         Rf42.shall.DLI=df.split[[1]]$Rf42.shallow*0.0864,
                         Rf42.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                         Rf42.deep.DLI=df.split[[1]]$Rf42.deep*0.0864,
                         HIMB.shall.DLI=df.split[[1]]$HIMB.shallow*0.0864,
                         HIMB.mid.DLI=df.split[[1]]$HIMB.mid*0.0864,
                         HIMB.deep.DLI=df.split[[1]]$HIMB.deep*0.0864),
              by=list(Date=df.split[[1]]$timestamp), FUN=mean)

df.dli<-df.dli[!(df.dli$Date=="2016-10-07"), ] # remove dates without full 24 h deployment
df.dli<-df.dli[!(df.dli$Date >="2016-10-18"), ]

colnames(df.dli)
# write.csv(df.dli, "DLI_Oct.csv")

#########################
# figures
#########################

pdf(paste("Oct.PAR",".pdf", sep=""))

# Reef 44

# Timespan
# the mean here is a dummy variable, need the "date" and "mean" for the figures
Timespan.df<-data.frame(df.dli[, (1:2)]); colnames(Timespan.df)<-c("Date", "DLI")

reefcols <- c("#FFA500", "#8968CD", "#C71585")
par(mfrow=c(2,2), mar=c(2,4,1,1), mgp=c(2,0.5,0))

k=1; lwd=1 # k-day moving averages
plot(DLI ~ Date, Timespan.df, type="n", ylab=(expression(paste("DLI mol photons", ~m^-2, ~d^-1, sep=""))), ylim=c(0, 55), xaxt="n", xlab="",  main="Reef 44 PAR")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf44.shall.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf44.mid.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[2], lwd=2) 
})

#########################
# Reef 42
plot(DLI ~ Date, Timespan.df, type="n", ylab=(expression(paste("DLI mol photons", ~m^-2, ~d^-1, sep=""))), ylim=c(0, 55), xaxt="n", xlab="",  main="Reef 42 PAR")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf42.shall.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf42.mid.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf42.deep.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[3], lwd=2) 
})

#########################
# HIMB
plot(DLI ~ Date, Timespan.df, type="n", ylab=(expression(paste("DLI mol", ~m^-2, ~d^-1, sep=""))), ylim=c(0, 55), xaxt="n", xlab="",  main="HIMB PAR")
mtext(expression(bold("D")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$HIMB.shall.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$HIMB.mid.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$HIMB.deep.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[3], lwd=2) 
})

dev.off()

