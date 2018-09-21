# light loggers pan KBay
# across depths and sites OCTOBER and November 2016

library(reshape2)
library(plyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(plotrix)
library(devtools)
library(tools)



#clear work space
rm(list=ls())
ls()

getwd()

##########################################
###### October Deployment ################
##########################################

##### grab files in a list
calibration.files <- list.files(path="data/environmental/temp and light/cross depth PAR temp/Oct 2016/light", pattern = "CSV$", full.names = T)
calibration.files

##### what are the file names, sans extensions
file.names<-file_path_sans_ext(list.files(path="data/environmental/temp and light/cross depth PAR temp/Oct 2016/light", pattern = "CSV$", full.names = F))

############ formatting all data in for loop
for(i in 1:length(calibration.files))
{
  data<-read.csv(calibration.files[i], sep=",", skip=8)
  df<-data[, c(-1,-5)] # removes "trash" columns
  colnames(df)<-c("Date", "Time", "Raw.PAR")
  df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
  df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
  df<-df[ , c(4,3)] # timestamp and Raw value only
  df<-df[!(df$timestamp < "2016-10-07 14:00:00"),] # start at this time
  df<-df[!(df$timestamp >= "2016-10-19 17:00:00"),] # end at this time
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
  # makes each df[i] as dataframe with specific file-name
  # write.csv(df.out, file=paste("trim",file.names[i])) # makes .csvs for output
}
# this is the end of the loop

########## see the files you've made, as a list, then grab those that fill your pattern 
ls()
files<-as.data.frame(mget(ls(pattern = "SN.*"))) # with SN as patterns in files from for loops
names(files) # see number of columns, and what these are

data_index<-c(1,(seq(2,16,2))) # these are the columns we will want: timestamp + raw data **change '18' to number of columns in your dataframe, specifying here to select 'every other column'

Oct.PAR<-as.data.frame(c(files[, data_index])) # here is the data we want, now in df alone
names(Oct.PAR) = gsub(pattern = "_.*", replacement = "", x = names(Oct.PAR)) #strip name to SN only
colnames(Oct.PAR)[1]="timestamp" # rename the single column for time

### apply data callibration

Oct.PAR$SN2484<-(Oct.PAR$SN2484*0.07403) # Light logger 1: SN 2484, Reef 44, <1m
Oct.PAR$SN2488<-(Oct.PAR$SN2488*0.07519) # Light logger 4: SN 2488, Reef 42, <1m
Oct.PAR$SN4322<-(Oct.PAR$SN4322*0.09929) # Light logger 2m: SN 4322, Reef 42
Oct.PAR$SN4375<-(Oct.PAR$SN4375*0.05762) # Light logger 2m: SN 4375, Reef 44
Oct.PAR$SN4804<-(Oct.PAR$SN4804*0.05199) # Light logger 8: SN 4804, HIMB, >7m
Oct.PAR$SN4805<-(Oct.PAR$SN4805*0.07131) # Light logger 2m: SN 4805, HIMB
Oct.PAR$SN4807<-(Oct.PAR$SN4807*0.03904) # Light logger 7: SN 4807, HIMB, <1m
Oct.PAR$SN6378<-(Oct.PAR$SN6378*0.2022)  # Light logger 3: SN 6378 Reef 42, >7m

# organize dataframe to relect site and depth
colnames(Oct.PAR)<-c("timestamp", "Rf44.shallow", "Rf42.shallow", "Rf42.mid", "Rf44.mid", "HIMB.deep",
                     "HIMB.mid", "HIMB.shallow", "Rf42.deep")

Oct.PAR<-Oct.PAR[c("timestamp", "Rf44.shallow", "Rf44.mid", "Rf42.shallow", "Rf42.mid", "Rf42.deep", 
                 "HIMB.shallow", "HIMB.mid", "HIMB.deep")] # reorder columns from N>S and shallow to deep


max(Oct.PAR$Rf44.shallow)
Oct.PAR$Rf44.shallow[which(Oct.PAR$Rf44.shallow > 2100)] <- 1863 # possible air exposure, set to nearest max

write.csv(Oct.PAR, "data/environmental/temp and light/Field.allPAR.Oct2016.csv")

# Check the 15-min calibrated data
par(mfrow=c(3,3), mar=c(5,4,1,2))
for(i in 1:length(1:9)) {
  y=Oct.PAR[,i]
  x=Oct.PAR$timestamp
  plot(y~x, data=df, ylab=colnames(Oct.PAR)[i], xlab="Date-time")
}

############################
# calculate daily integrated light values for each logger
# multiply calibrated values (umol photons m-2 s-1) by 0.0864 to reach mol photons m-2 s-1
# umol/s.. /1,000,000 * 24h * 60m * 60s = 0.0864... 24h light integral
# umol/s.. /1,000,000 * 12h * 60m * 60s = 0.0432... 12h light integral
# since averaging over full 24 h period in situ, best way is to get DLI across 24h period 

df<-Oct.PAR
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
write.csv(df.dli, "data/environmental/temp and light/DLI.Oct2016.csv")

##########################################
###### November Deployment ################
##########################################
rm(list=ls())
ls()

calibration.files <- list.files(path="data/environmental/temp and light/cross depth PAR temp/Nov 2016/light", pattern = "CSV$", full.names = T); calibration.files

file.names<-file_path_sans_ext(list.files(path="data/environmental/temp and light/cross depth PAR temp/Nov 2016/light", pattern = "CSV$", full.names = F)); file.names

for(i in 1:length(calibration.files))
{
  data<-read.csv(calibration.files[i], sep=",", skip=8)
  df<-data[, c(-1,-5)] # removes "trash" columns
  colnames(df)<-c("Date", "Time", "Raw.PAR")
  df$Date<-parse_date_time(df$Date, "dmy") # corrects date format
  df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
  df<-df[ , c(4,3)] # timestamp and Raw value only
  df<-df[!(df$timestamp < "2016-11-08 13:00:00"),] # start at this time
  df<-df[!(df$timestamp >= "2016-11-20 12:00:00"),] # end at this time
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
}

ls()
files<-as.data.frame(mget(ls(pattern = "SN.*")))
names(files)

data_index<-c(1,(seq(2,12,2))) 
Nov.PAR<-as.data.frame(c(files[, data_index]))
names(Nov.PAR) = gsub(pattern = "_.*", replacement = "", x = names(Nov.PAR))
colnames(Nov.PAR)[1]="timestamp"

### apply data callibration
Nov.PAR$SN2484<-(Nov.PAR$SN2484*0.07403) # Light logger 1: SN 2484, Reef 44, <1m
Nov.PAR$SN4375<-(Nov.PAR$SN4375*0.05762) # Light logger Raph: SN 4375, Reef 44, 2m
Nov.PAR$SN4802<-(Nov.PAR$SN4802*0.06099) # Light logger 2: SN 4802, Reef 44, >7m
Nov.PAR$SN4804<-(Nov.PAR$SN4804*0.03904) # Light logger 8: SN 4804, Reef 10, 2m
Nov.PAR$SN4807<-(Nov.PAR$SN4807*0.05199) # Light logger 7: SN 4807, Reef 10, >7m
Nov.PAR$SN6377<-(Nov.PAR$SN6377*0.1149)  # Light logger 5: SN 6377, Reef 10, <1m

colnames(Nov.PAR)<-c("timestamp", "Rf44.shallow", "Rf44.mid", "Rf44.deep", "Rf10.mid", "Rf10.deep", "Rf10.shallow")

Nov.PAR<-Nov.PAR[c("timestamp", "Rf44.shallow", "Rf44.mid", "Rf44.deep", "Rf10.shallow", "Rf10.mid", "Rf10.deep")] # reorder columns from N>S and shallow to deep

write.csv(Nov.PAR, "data/environmental/temp and light/Field.allPAR.Nov2016.csv")

# Check the 15-min calibrated data
par(mfrow=c(3,3), mar=c(5,4,1,2))
for(i in 1:length(1:7)) {
  y=Nov.PAR[,i]
  x=Nov.PAR$timestamp
  plot(y~x, data=df, ylab=colnames(Nov.PAR)[i], xlab="Date-time")
}

################
# calculate daily integrated light values for each logger
df<-Nov.PAR
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp) # change to DATE ONLY format to calulate range, means
str(df)

df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-07", format="%F")) # split df by date

df.dli<-aggregate(data.frame(Rf44.shall.DLI=df.split[[1]]$Rf44.shallow*0.0864,
                             Rf44.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                             Rf44.deep.DLI=df.split[[1]]$Rf44.deep*0.0864,
                             Rf10.shall.DLI=df.split[[1]]$Rf10.shallow*0.0864,
                             Rf10.mid.DLI=df.split[[1]]$Rf10.mid*0.0864,
                             Rf10.deep.DLI=df.split[[1]]$Rf10.deep*0.0864),
                  by=list(Date=df.split[[1]]$timestamp), FUN=mean)

df.dli<-df.dli[!(df.dli$Date=="2016-11-08"), ] # remove dates without full 24 h deployment
df.dli<-df.dli[!(df.dli$Date >="2016-11-20"), ]

colnames(df.dli)
write.csv(df.dli, "data/environmental/temp and light/DLI.Nov2016.csv")



#########################
# figures October
#########################
df.dli<-read.csv("data/environmental/temp and light/DLI.Oct2016.csv")
df.dli<-df.dli[,-1] # remove R imported junk column
df.dli$Date<-as.Date(df.dli$Date)

pdf(paste("figures/environmental/Oct.PAR",".pdf", sep=""))

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

#########################
# figures November
#########################
df.dli<-read.csv("data/environmental/temp and light/DLI.Nov2016.csv")
df.dli<-df.dli[,-1] # remove R imported junk column
df.dli$Date<-as.Date(df.dli$Date)

pdf(paste("figures/environmental/Nov.PAR",".pdf", sep=""))

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
with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf44.deep.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[3], lwd=2) 
})


#########################
# Reef 10
plot(DLI ~ Date, Timespan.df, type="n", ylab=(expression(paste("DLI mol photons", ~m^-2, ~d^-1, sep=""))), ylim=c(0, 55), xaxt="n", xlab="",  main="Reef 10 PAR")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf10.shall.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf10.mid.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=df.dli$Date, DLI=rollmean(df.dli$Rf10.deep.DLI, k, fill=NA))), { 
  lines(date, DLI, col=reefcols[3], lwd=2) 
})

dev.off()




####################################################
####################################################
### Light attenuation  October deployment ##########
####################################################
####################################################

# using October deployment at 2 depths

df<-read.csv("data/environmental/temp and light/Field.allPAR.Oct2016.csv")
df<-df[ , -which(names(df) %in% c("X","Rf44.shallow", "Rf44.mid"))] # remove R44 w/o 3 levels
df$timestamp<-as.POSIXct(df$timestamp)
df[df<=1] <- NA # remove low values to get day only

Oct.PAR<-df
colnames(Oct.PAR)[2:7]<-"PAR" # change all light columns to read "PAR" this is for rbind later

Rf42.shall<-Oct.PAR[1:2] #only time and PAR
Rf42.shall$Site<-as.factor("Rf42") # make site level
Rf42.shall$Depth<-3*0.3048 # logger at 3 ft, convert to --> meters

Rf42.mid<-Oct.PAR[, c(1,3)]; Rf42.mid$Site<-as.factor("Rf42"); Rf42.mid$Depth<-8*0.3048 # logger at 8 ft 
Rf42.deep<-Oct.PAR[, c(1,4)]; Rf42.deep$Site<-as.factor("Rf42"); Rf42.deep$Depth<-27*0.3048 # logger at 27 ft
HIMB.shall<-Oct.PAR[, c(1,5)]; HIMB.shall$Site<-as.factor("HIMB"); HIMB.shall$Depth<-1*0.3048 # logger at 1 ft
HIMB.mid<-Oct.PAR[, c(1,6)]; HIMB.mid$Site<-as.factor("HIMB"); HIMB.mid$Depth<-6*0.3048 # logger at 6 ft
HIMB.deep<-Oct.PAR[, c(1,7)]; HIMB.deep$Site<-as.factor("HIMB"); HIMB.deep$Depth<-25*0.3048 # logger at 25 ft

PAR.df<-rbind(Rf42.shall, Rf42.mid, Rf42.deep, HIMB.shall, HIMB.mid, HIMB.deep)

# Break up into each reef
#####
PAR.Rf42<-PAR.df[(PAR.df$Site=="Rf42"),] # just Rf42
PAR.Rf42$Depth<-as.factor(PAR.Rf42$Depth) # depth as factor
PAR.Rf42$log.PAR<-log(PAR.Rf42$PAR)

mod<-lm(log.PAR~Depth, data=PAR.Rf42, na.action=na.exclude) # Rf42 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf42.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])

shal<-Rf42.coeffs[1]; mid<-Rf42.coeffs[2]; deep<-Rf42.coeffs[3]

####
####
# make new dataframes for Sites
Rf42.df<-df[1:4]; 

####
# making some log data
Rf42.df$log.shal<-log(Rf42.df$Rf42.shallow)
Rf42.df$log.mid<-log(Rf42.df$Rf42.mid)
Rf42.df$log.deep<-log(Rf42.df$Rf42.deep)

# making some log data using coefficients
Rf42.df$coef.shal<-Rf42.df$log.shal
Rf42.df$coef.mid<-Rf42.df$log.shal+mid
Rf42.df$coef.deep<-Rf42.df$log.shal+deep

# in present form model coeffs are based on shallow ---> depth,  to calculate deep and shallow from mid...
Rf42.df$coef.shal2<-Rf42.df$log.mid-mid # log(midPAR)-mid(coeff) = log(shallow) <<<< use this definition below
Rf42.df$coef.mid2<-Rf42.df$log.mid # log(midPAR) is data you have

# log(shallow)+deep(coeff) = log(deep)...# log(midPAR)-mid(coeff)+deep(coeff)=log(deep)
Rf42.df$coef.deep2<-Rf42.df$log.mid-mid+deep


##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(3,1), mar=c(4,4,2,2))
plot(y=Rf42.df$log.shal, x=Rf42.df$timestamp, type="l", col="dodgerblue", 
     main="Reef 42 logger (top), <1m-coef (mid), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="log(PAR)", xlab="", ylim=c(0,8))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$log.mid, x=Rf42.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$log.deep, x=Rf42.df$timestamp, type="l", col="paleturquoise3"))

##### plot of modeled 2m and 8m using <1m model coeffs. 
plot(y=Rf42.df$coef.shal, x=Rf42.df$timestamp, type="l", col="dodgerblue", ylab="log(PAR)", 
     xlab="Date", ylim=c(0,8))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.mid, x=Rf42.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.deep, x=Rf42.df$timestamp, type="l", col="paleturquoise3"))

# plot of modeled <1m and 8m using 2m data
plot(y=Rf42.df$coef.shal2, x=Rf42.df$timestamp, type="l", col="dodgerblue", ylab="log(PAR)", 
     xlab="Date", ylim=c(0,8))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.mid2, x=Rf42.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.deep2, x=Rf42.df$timestamp, type="l", col="paleturquoise3"))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_data_2xcoef_Rf42.pdf", height=4, width=6)
dev.off()


################################ 
########### HIMB ############### 
PAR.HIMB<-PAR.df[(PAR.df$Site=="HIMB"),] # just HIMB
PAR.HIMB$Depth<-as.factor(PAR.HIMB$Depth) # depth as factor
mod<-lm(log(PAR)~Depth, data=PAR.HIMB, na.action=na.exclude) # HIMB dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
HIMB.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])
shal<-coeffs[1]; mid<-coeffs[2]; deep<-coeffs[3]

####
####
# make new dataframes for Sites
HIMB.df<-df[, c(1,5:7)]; 
HIMB.df[HIMB.df<=1] <- NA # remove leftover values that remain low independently

####
# making some log data
HIMB.df$log.shal<-log(HIMB.df$HIMB.shallow)
HIMB.df$log.mid<-log(HIMB.df$HIMB.mid)
HIMB.df$log.deep<-log(HIMB.df$HIMB.deep)

# making some log data using coefficients
HIMB.df$coef.shal<-HIMB.df$log.shal
HIMB.df$coef.mid<-HIMB.df$log.shal+mid
HIMB.df$coef.deep<-HIMB.df$log.shal+deep

# using coeffs. calculate deep and shallow from mid
HIMB.df$coef.shal2<-HIMB.df$log.mid-mid # log(midPAR)-mid(coeff) = log(shallow) <<<< use this definition below
HIMB.df$coef.mid2<-HIMB.df$log.mid # log(midPAR) is data you have
HIMB.df$coef.deep2<-HIMB.df$log.mid-mid+deep

##################
##### Figure #####

# plot log data
par(mfrow=c(3,1), mar=c(4,4,2,2))
plot(y=HIMB.df$log.shal, x=HIMB.df$timestamp, type="l", col="mediumorchid2", 
     main="HIMB logger (top), <1m-coef (mid), 2m-coeff (bottom)", cex.main=0.7, ylab="log(PAR)", 
     xlab="", ylim=c(0,8))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$log.mid, x=HIMB.df$timestamp, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$log.deep, x=HIMB.df$timestamp, type="l", col="plum4"))

# plot of modeled 2m and 8m using <1m model coeffs. 
plot(y=HIMB.df$coef.shal, x=HIMB.df$timestamp, type="l", col="mediumorchid2", 
     cex.main=0.7, ylab="log(PAR)", xlab="", ylim=c(0,8))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.mid, x=HIMB.df$timestamp, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.deep, x=HIMB.df$timestamp, type="l", col="plum4"))

# plot of modeled <1m and 8m using 2m data
plot(y=HIMB.df$coef.shal2, x=HIMB.df$timestamp, type="l", col="mediumorchid2", 
     ylab="log(PAR)", xlab="Date", ylim=c(0,8))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.mid2, x=HIMB.df$timestamp, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.deep2, x=HIMB.df$timestamp, type="l", col="plum4"))
legend("top", lty=1, col=c("mediumorchid2", "gray", "plum4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_data_2xcoef_HIMB.pdf", height=4, width=6)
dev.off()


####################################################
####################################################
### Light attenuation  November deployment #########
####################################################
####################################################

# using November deployment at 2 depths

df<-read.csv("data/environmental/temp and light/Field.allPAR.Nov2016.csv"); df<-df[-1]
df$timestamp<-as.POSIXct(df$timestamp)
df[df<=1] <- NA # remove low values to get day only

Nov.PAR<-df
colnames(Nov.PAR)[2:7]<-"PAR" # change all light columns to read "PAR" this is for rbind later

Rf44.shall<-Nov.PAR[1:2] #only time and PAR
Rf44.shall$Site<-as.factor("Rf44") # make site level
Rf44.shall$Depth<-1*0.3048 # logger at 1 ft, convert to --> meters

Rf44.mid<-Nov.PAR[, c(1,3)]; Rf44.mid$Site<-as.factor("Rf44"); Rf44.mid$Depth<-6*0.3048 # logger at 6 ft 
Rf44.deep<-Nov.PAR[, c(1,4)]; Rf44.deep$Site<-as.factor("Rf44"); Rf44.deep$Depth<-25*0.3048 # logger at 25 ft
Rf10.shall<-Nov.PAR[, c(1,5)]; Rf10.shall$Site<-as.factor("Rf10"); Rf10.shall$Depth<-2*0.3048 # logger at 1 ft
Rf10.mid<-Nov.PAR[, c(1,6)]; Rf10.mid$Site<-as.factor("Rf10"); Rf10.mid$Depth<-6*0.3048 # logger at 6 ft
Rf10.deep<-Nov.PAR[, c(1,7)]; Rf10.deep$Site<-as.factor("Rf10"); Rf10.deep$Depth<-26*0.3048 # logger at 26 ft

# bind together
PAR.df<-rbind(Rf44.shall, Rf44.mid, Rf44.deep, Rf10.shall, Rf10.mid, Rf10.deep)

# Break up into each reef
#####
PAR.Rf44<-PAR.df[(PAR.df$Site=="Rf44"),] # just Rf44
PAR.Rf44$Depth<-as.factor(PAR.Rf44$Depth) # depth as factor
PAR.Rf44$log.PAR<-log(PAR.Rf44$PAR)

mod<-lm(log.PAR~Depth, data=PAR.Rf44, na.action=na.exclude) # R44 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf44.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])

shal<-Rf44.coeffs[1]; mid<-Rf44.coeffs[2]; deep<-Rf44.coeffs[3]

####
####
# make new dataframes for Sites
Rf44.df<-df[1:4]; 

####
# making some log data
Rf44.df$log.shal<-log(Rf44.df$Rf44.shallow)
Rf44.df$log.mid<-log(Rf44.df$Rf44.mid)
Rf44.df$log.deep<-log(Rf44.df$Rf44.deep)

# making some log data using coefficients
Rf44.df$coef.shal<-Rf44.df$log.shal
Rf44.df$coef.mid<-Rf44.df$log.shal+mid
Rf44.df$coef.deep<-Rf44.df$log.shal+deep

# in present form model coeffs are based on shallow ---> depth,  to calculate deep and shallow from mid...
Rf44.df$coef.shal2<-Rf44.df$log.mid-mid # log(midPAR)-mid(coeff) = log(shallow) <<<< use this definition below
Rf44.df$coef.mid2<-Rf44.df$log.mid # log(midPAR) is data you have

# log(shallow)+deep(coeff) = log(deep)...# log(midPAR)-mid(coeff)+deep(coeff)=log(deep)
Rf44.df$coef.deep2<-Rf44.df$log.mid-mid+deep


##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(3,1), mar=c(4,4,2,2))
plot(y=Rf44.df$log.shal, x=Rf44.df$timestamp, type="l", col="yellowgreen", 
     main="Reef 44 logger (top), <1m-coef (mid), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="log(PAR)", xlab="", ylim=c(0,8))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$log.mid, x=Rf44.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$log.deep, x=Rf44.df$timestamp, type="l", col="palegreen4"))

##### plot of modeled 2m and 8m using <1m model coeffs. 
plot(y=Rf44.df$coef.shal, x=Rf44.df$timestamp, type="l", col="yellowgreen", ylab="log(PAR)", 
     xlab="", ylim=c(0,8))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.mid, x=Rf44.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.deep, x=Rf44.df$timestamp, type="l", col="palegreen4"))

# plot of modeled <1m and 8m using 2m data
plot(y=Rf44.df$coef.shal2, x=Rf44.df$timestamp, type="l", col="yellowgreen", ylab="log(PAR)", 
     xlab="Date", ylim=c(0,8))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.mid2, x=Rf44.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.deep2, x=Rf44.df$timestamp, type="l", col="palegreen4"))
legend("top", lty=1, col=c("yellowgreen", "gray", "palegreen4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_data_2xcoef_Rf44.pdf", height=4, width=6)
dev.off()


###################################
########### Reef 10 ############### 
PAR.Rf10<-PAR.df[(PAR.df$Site=="Rf10"),] # just Rf10
PAR.Rf10$Depth<-as.factor(PAR.Rf10$Depth) # depth as factor
mod<-lm(log(PAR)~Depth, data=PAR.Rf10, na.action=na.exclude) # Rf10 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf10.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])
shal<-coeffs[1]; mid<-coeffs[2]; deep<-coeffs[3]

####
####
# make new dataframes for Sites
Rf10.df<-df[, c(1,5:7)]

####
# making some log data
Rf10.df$log.shal<-log(Rf10.df$Rf10.shallow)
Rf10.df$log.mid<-log(Rf10.df$Rf10.mid)
Rf10.df$log.deep<-log(Rf10.df$Rf10.deep)

# making some log data using coefficients
Rf10.df$coef.shal<-Rf10.df$log.shal
Rf10.df$coef.mid<-Rf10.df$log.shal+mid
Rf10.df$coef.deep<-Rf10.df$log.shal+deep

# using coeffs. calculate deep and shallow from mid
Rf10.df$coef.shal2<-Rf10.df$log.mid-mid # log(midPAR)-mid(coeff) = log(shallow) <<<< use this definition below
Rf10.df$coef.mid2<-Rf10.df$log.mid # log(midPAR) is data you have
Rf10.df$coef.deep2<-Rf10.df$log.mid-mid+deep

##################
##### Figure #####

# plot log data
par(mfrow=c(3,1), mar=c(4,4,2,2))
plot(y=Rf10.df$log.shal, x=Rf10.df$timestamp, type="l", col="orangered", 
     main="Reef 10 logger (top), <1m-coef (mid), 2m-coeff (bottom)", cex.main=0.7, ylab="log(PAR)", 
     xlab="", ylim=c(0,8))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$log.mid, x=Rf10.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$log.deep, x=Rf10.df$timestamp, type="l", col="rosybrown2"))

# plot of modeled 2m and 8m using <1m model coeffs. 
plot(y=Rf10.df$coef.shal, x=Rf10.df$timestamp, type="l", col="orangered", 
     cex.main=0.7, ylab="log(PAR)", xlab="", ylim=c(0,8))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.mid, x=Rf10.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.deep, x=Rf10.df$timestamp, type="l", col="rosybrown2"))

# plot of modeled <1m and 8m using 2m data
plot(y=Rf10.df$coef.shal2, x=Rf10.df$timestamp, type="l", col="orangered", 
     ylab="log(PAR)", xlab="Date", ylim=c(0,8))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.mid2, x=Rf10.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.deep2, x=Rf10.df$timestamp, type="l", col="rosybrown2"))
legend("top", lty=1, col=c("orangered", "gray", "rosybrown2"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.45), seg.len=1, cex=0.9, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_data_2xcoef_Rf10.pdf", height=4, width=6)
dev.off()

########
########
# combine all coefficients
all.coefficients<-rbind(Rf42.coeffs, HIMB.coeffs, Rf44.coeffs, Rf10.coeffs)
write.csv(all.coefficients, "data/environmental/light coeffs.csv")

###########
###########
########### 
# read in all PAR data at 2m and model what the 1m and 8m data would look like
All.PAR<-read.csv("data/environmental/temp and light/Jun_DecPAR/All.PAR.csv")
