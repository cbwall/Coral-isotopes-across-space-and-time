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
library(tools)

# remove all unwanted rows and columns
# split date and time column

##### grab files in a list
files <- list.files(path="data/environmental/temp and light/cross depth PAR temp/Nov 2016/temp/", pattern = "csv$", full.names = T)
files

##### what are the file names, sans extensions
file.names<-file_path_sans_ext(list.files(path="data/environmental/temp and light/cross depth PAR temp/Nov 2016/temp", pattern = "csv$", full.names = F))
file.names


############ formatting all data in for loop
for(i in 1:length(files))
{
  data<-read.csv(files[i], sep=",")
  df<-data[c(-1:-2),-1] # removes "trash" columns
  df<-df[, c(1:2)] # reorganize
  colnames(df)<-c("Date.time", "Raw.Temp")
  df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))
  df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
  df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
  df<-df[, c(3,2)] # remove old date-time column
  df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
  df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
  # makes each df[i] as dataframe with specific file-name
  # write.csv(df.out, file=paste("trim",file.names[i])) # makes .csvs for output
}
# this is the end of the loop

########## see the files you've made, as a list, then grab those that fill your pattern 
ls()
files<-as.data.frame(mget(ls(pattern = "SN.*"))) # with SN as patterns in files from for loops
names(files) # see number of columns, and what these are

data_index<-c(1,(seq(2,12,2))) # these are the columns we will want: timestamp + raw data **change '12' to number of columns in your dataframe, specifying here to select 'every other column'

temps<-as.data.frame(c(files[, data_index])) # here is the data we want, now in df alone
names(temps) = gsub(pattern = "_.*", replacement = "", x = names(temps)) #strip name to SN only
colnames(temps)[1]="timestamp" # rename the single column for time

### apply data callibration to each column
# ex: temp$calib.SN10209515<-(temp$SN10209515 * "value") # Temp logger x: SN x

write.csv(temp, "xxx.csv")


# Temp1_Nov2016: SN 10487936
#####################
data<-read.csv("data/environmental/temp and light/cross depth PAR temp/Nov 2016/temp/10487936_Temp1_Nov2016.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*0.9994+0.09144)

head(df)
temp1_10487936<-df
#####################

# Temp2_Nov_2016: SN 10779057
#####################
data<-read.csv("data/cross depth PAR temp/Nov 2016/Temp2_Nov2016_10779057.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.041-0.9401)

head(df)
temp2_10779057<-df
#####################

# Temp3_Nov_2016: SN 10779058
#####################
data<-read.csv("data/cross depth PAR temp/Nov 2016/Temp3_Nov2016_10779058.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.036-0.8501)

head(df)
temp3_10779058<-df
#####################


# Temp4_Nov_2016: SN 10779068
#####################
data<-read.csv("data/cross depth PAR temp/Nov 2016/Temp4_Nov2016_10779068.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.027-0.7018)

head(df)
temp4_10779068<-df
#####################

# Temp5_Nov_2016: SN 10487930
#####################
data<-read.csv("data/cross depth PAR temp/Nov 2016/Temp5_Nov2016_10487930.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.048-1.105)

head(df)
temp5_10487930<-df
#####################

# Temp6_Nov_2016: SN 10487935
#####################
data<-read.csv("data/cross depth PAR temp/Nov 2016/Temp6_Nov2016_10487935.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.028-0.6453)

head(df)
temp6_10487935<-df
#####################

# Temp7_Nov_2016: SN 10779066
#####################
data<-read.csv("data/cross depth PAR temp/Nov 2016/Temp7_Nov2016_10779066.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*1.023-0.5129)

head(df)
temp7_10779066<-df
#####################

## no logger 8--not deployed

#####################

# logger 9: Rf44_Nov8_10209515.csv
#####################
data<-read.csv("data/cross depth PAR temp/Nov 2016/Rf44_Nov8_10209515.csv")
df<-data[c(-1:-2),-1] # removes "trash" rows and columns
df<-df[, c(1:2)] 
colnames(df)<-c("Date.time", "Raw.Temp")
df$Raw.Temp<-as.numeric(as.character(df$Raw.Temp))

df$timestamp<-mdy_hms(as.character(df$Date.time)) # corrects date format
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df<-df[, c(3,2)] # remove old date-time column
df<-df[!(df$timestamp < "2016-11-08 14:00:00"),] # start at this time
df<-df[!(df$timestamp > "2016-12-09 09:00:00"),] # end at this time

df$Cal_Temp<-((df$Raw.Temp)*0.9762+0.5346)

head(df)
temp9_10209515<-df
#####################

# logger 10: Rf20_Nov8_9768609

# no reef 20 data, logger flooded

#####################

#######################
# make one large dataframe with date, time, and calibration mean for all loggers
ALL.temps<-cbind(temp1_10487936[c(1,3)], temp2_10779057[c(0,3)], temp3_10779058[c(0,3)], temp4_10779068[c(0,3)], temp5_10487930[c(0,3)], temp6_10487935[c(0,3)], temp7_10779066[c(0,3)], temp9_10209515[c(0,3)])

colnames(ALL.temps)<-c("timestamp", "temp1_10487936_CalTemp", "temp2_10779057_CalTemp", "temp3_10779058_CalTemp", "temp4_10779068_CalTemp", "temp5_10487930_CalTemp", "temp6_10487935_CalTemp", "temp7_10779066_CalTemp", "temp9_10209515_CalTemp")

ALL.temps$timestamp<-as.POSIXct(ALL.temps$timestamp) # need to do this for graphing data

write.csv(ALL.temps,"Field.alltemps.Nov2016.csv")

# figures
par(mfrow=c(1,1))
plot(ALL.temps$temp1_10487936_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp2_10779057_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp3_10779058_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp4_10779068_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp5_10487930_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp6_10487935_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp7_10779066_CalTemp~ALL.temps$timestamp)
plot(ALL.temps$temp9_10209515_CalTemp~ALL.temps$timestamp)


#########################
#########################
# organize dataframe "ALL.temps" to relect site and depth
colnames(ALL.temps)
colnames(ALL.temps)<-c("timestamp", "Rf44.shallow", "Rf44.deep","Rf20.shallow", "Rf20.deep", "Rf10.shallow", "Rf10.mid", "Rf10.deep", "Rf44.mid")

ALL.temps<-ALL.temps[, c(1:2,9,3:5,6:8)] #reorder columns from N>S and shallow > deep
head(ALL.temps)

df<-ALL.temps # rename dataframe
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp) # change to DATE ONLY format to calulate range, means
df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-10", format="%F")) # split df by date

head(df)
# calculate daily means, aggregate into one dataframe
df.mean <- aggregate(data.frame(Rf44.shall.mean=df.split[[1]]$Rf44.shallow,
                                Rf44.mid.mean=df.split[[1]]$Rf44.mid,
                                Rf44.deep.mean=df.split[[1]]$Rf44.deep,
                                Rf20.shall.mean=df.split[[1]]$Rf20.shallow,
                                Rf20.deep.mean=df.split[[1]]$Rf20.deep,
                                Rf10.shall.mean=df.split[[1]]$Rf10.shallow,
                                Rf10.mid.mean=df.split[[1]]$Rf10.mid,
                                Rf10.deep.mean=df.split[[1]]$Rf10.deep) ,
                     by=list(Date=df.split[[1]]$timestamp), FUN=mean)

# calculate daily maximum, aggregate into one dataframe
df.max <- aggregate(data.frame(Rf44.shall.max=df.split[[1]]$Rf44.shallow,
                               Rf44.mid.max=df.split[[1]]$Rf44.mid,
                               Rf44.deep.max=df.split[[1]]$Rf44.deep,
                               Rf20.shall.max=df.split[[1]]$Rf20.shallow,
                               Rf20.deep.max=df.split[[1]]$Rf20.deep,
                               Rf10.shall.max=df.split[[1]]$Rf10.shallow,
                               Rf10.mid.max=df.split[[1]]$Rf10.mid,
                               Rf10.deep.max=df.split[[1]]$Rf10.deep) ,
                    by=list(Date=df.split[[1]]$timestamp), FUN=max)

# calculate daily minumum, aggregate into one dataframe
df.min <- aggregate(data.frame(Rf44.shall.min=df.split[[1]]$Rf44.shallow,
                               Rf44.mid.min=df.split[[1]]$Rf44.mid,
                               Rf44.deep.min=df.split[[1]]$Rf44.deep,
                               Rf20.shall.min=df.split[[1]]$Rf20.shallow,
                               Rf20.deep.min=df.split[[1]]$Rf20.deep,
                               Rf10.shall.min=df.split[[1]]$Rf10.shallow,
                               Rf10.mid.min=df.split[[1]]$Rf10.mid,
                               Rf10.deep.min=df.split[[1]]$Rf10.deep) ,
                    by=list(Date=df.split[[1]]$timestamp), FUN=min)

# calculate daily range, aggregate into one dataframe
df.range <- data.frame(timestamp=df.max[[1]], 
                       Rf44.shall.range=df.max$Rf44.shall.max-df.min$Rf44.shall.min,
                       Rf44.mid.range=df.max$Rf44.mid.max-df.min$Rf44.mid.min,
                       Rf44.deep.range=df.max$Rf44.deep.max-df.min$Rf44.deep.min,
                       Rf20.shall.range=df.max$Rf20.shall.max-df.min$Rf20.shall.min,
                       Rf20.deep.range=df.max$Rf20.deep.max-df.min$Rf20.deep.min,
                       Rf10.shall.range=df.max$Rf10.shall.max-df.min$Rf10.shall.min,
                       Rf10.mid.range=df.max$Rf10.mid.max-df.min$Rf10.mid.min,
                       Rf10.deep.range=df.max$Rf10.deep.max-df.min$Rf10.deep.min)

write.csv(df.range, "Nov.temp range.csv")
##############################
# make dataframe for each reef

# Reef 44
Rf44.temp.Nov<-data.frame(df.mean[, c(1:4)], df.max[, c(2:4)], df.min[, c(2:4)], df.range[, c(2:4)])

# Reef 20
Rf20.temp.Nov<-data.frame(df.mean[, c(1,5:6)], df.max[, c(5:6)], df.min[, c(5:6)], df.range[, c(5:6)])

# Reef 10
Rf10.temp.Nov<-data.frame(df.mean[, c(1,7:9)], df.max[, c(7:9)], df.min[, c(7:9)], df.range[, c(7:9)])


#########################
# figures
#########################

pdf(paste("Nov.temps",".pdf", sep=""))

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

with(na.omit(data.frame(date=Rf44.temp.Nov$Date, mean=rollmean(Rf44.temp.Nov$Rf44.shall.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Nov$Date, mean=rollmean(Rf44.temp.Nov$Rf44.mid.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Nov$Date, mean=rollmean(Rf44.temp.Nov$Rf44.deep.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[3], lwd=2)
})

#########################
# Reef 20
plot(mean ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 20 Temp mean")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf20.temp.Nov$Date, mean=rollmean(Rf20.temp.Nov$Rf20.shall.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf20.temp.Nov$Date, mean=rollmean(Rf20.temp.Nov$Rf20.deep.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[3], lwd=2)
})

#########################
# Reef 10
plot(mean ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 10 Temp mean")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf10.temp.Nov$Date, mean=rollmean(Rf10.temp.Nov$Rf10.shall.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf10.temp.Nov$Date, mean=rollmean(Rf10.temp.Nov$Rf10.mid.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf10.temp.Nov$Date, mean=rollmean(Rf10.temp.Nov$Rf10.deep.mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[3], lwd=2)
})



# MAXIMUM TEMPERATURES
#########################

# Reef 44
par(mfrow=c(2,2), mar=c(2,4,1,1), mgp=c(2,0.5,0))
colnames(Timespan.df)<-c("Date", "max") # change to 'max' to match dataframe here

plot(max ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 44 Temp max")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf44.temp.Nov$Date, max=rollmean(Rf44.temp.Nov$Rf44.shall.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Nov$Date, max=rollmean(Rf44.temp.Nov$Rf44.mid.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Nov$Date, max=rollmean(Rf44.temp.Nov$Rf44.deep.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[3], lwd=2)
})

#########################
# Reef 20
plot(max ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 20 Temp max")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf20.temp.Nov$Date, max=rollmean(Rf20.temp.Nov$Rf20.shall.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf20.temp.Nov$Date, max=rollmean(Rf20.temp.Nov$Rf20.deep.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[3], lwd=2)
})

#########################
# Reef 10
plot(max ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 10 Temp max")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m /%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf10.temp.Nov$Date, max=rollmean(Rf10.temp.Nov$Rf10.shall.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf10.temp.Nov$Date, max=rollmean(Rf10.temp.Nov$Rf10.mid.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf10.temp.Nov$Date, max=rollmean(Rf10.temp.Nov$Rf10.deep.max, k, fill=NA))), { 
  lines(date, max, col=reefcols[3], lwd=2)
})



# MINIMUM TEMPERATURES
#########################

# Reef 44
colnames(Timespan.df)<-c("Date", "min")  # change to 'min' to match dataframe here
par(mfrow=c(2,2), mar=c(2,4,1,1), mgp=c(2,0.5,0))

plot(min ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 44 Temp min")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf44.temp.Nov$Date, min=rollmean(Rf44.temp.Nov$Rf44.shall.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Nov$Date, min=rollmean(Rf44.temp.Nov$Rf44.mid.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Nov$Date, min=rollmean(Rf44.temp.Nov$Rf44.deep.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[3], lwd=2)
})

#########################
# Reef 20
plot(min ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 20 Temp min")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf20.temp.Nov$Date, min=rollmean(Rf20.temp.Nov$Rf20.shall.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf20.temp.Nov$Date, min=rollmean(Rf20.temp.Nov$Rf20.deep.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[3], lwd=2)
})

#########################
# Reef 10
plot(min ~ Date, Timespan.df, type="n", ylab="Temperature (°C)", ylim=c(23, 29), xaxt="n", xlab="",  main="Reef 10 Temp min")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf10.temp.Nov$Date, min=rollmean(Rf10.temp.Nov$Rf10.shall.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf10.temp.Nov$Date, min=rollmean(Rf10.temp.Nov$Rf10.mid.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf10.temp.Nov$Date, min=rollmean(Rf10.temp.Nov$Rf10.deep.min, k, fill=NA))), { 
  lines(date, min, col=reefcols[3], lwd=2)
})




# RANGE TEMPERATURES
#########################

# Reef 44
colnames(Timespan.df)<-c("Date", "range") # change to 'range' to match dataframe here
par(mfrow=c(2,2), mar=c(2,4,1,1), mgp=c(2,0.5,0))

plot(range ~ Date, Timespan.df, type="n", ylab=Delta~"Temperature (°C)"~day^-1, ylim=c(0, 4), xaxt="n", xlab="",  main="Reef 44 Temp range")
mtext(expression(bold("A")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf44.temp.Nov$Date, range=rollmean(Rf44.temp.Nov$Rf44.shall.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Nov$Date, range=rollmean(Rf44.temp.Nov$Rf44.mid.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf44.temp.Nov$Date, range=rollmean(Rf44.temp.Nov$Rf44.deep.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[3], lwd=2)
})

#########################
# Reef 20
plot(range ~ Date, Timespan.df, type="n", ylab=Delta~"Temperature (°C)"~day^-1, ylim=c(0, 4), xaxt="n", xlab="",  main="Reef 20 Temp range")
mtext(expression(bold("B")), 2, adj=-1, las=1, padj=-10.5)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf20.temp.Nov$Date, range=rollmean(Rf20.temp.Nov$Rf20.shall.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf20.temp.Nov$Date, range=rollmean(Rf20.temp.Nov$Rf20.deep.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[3], lwd=2)
})

#########################
# Reef 10
plot(range ~ Date, Timespan.df, type="n", ylab=Delta~"Temperature (°C)"~day^-1, ylim=c(0, 4), xaxt="n", xlab="",  main="Reef 10 Temp range")
mtext(expression(bold("C")), 2, adj=-1, las=1, padj=-10)
axis.Date(1, at=seq(min(Timespan.df$Date), max(Timespan.df$Date), by="1 day"), format="%m/%d/%y")
legend("topright", lty=1, col=c(reefcols[1:3]), legend=c("< 1 m","2 m", "7 m"), lwd=3, bty="n")

with(na.omit(data.frame(date=Rf10.temp.Nov$Date, range=rollmean(Rf10.temp.Nov$Rf10.shall.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=2) 
})
with(na.omit(data.frame(date=Rf10.temp.Nov$Date, range=rollmean(Rf10.temp.Nov$Rf10.mid.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[2], lwd=2) 
})
with(na.omit(data.frame(date=Rf10.temp.Nov$Date, range=rollmean(Rf10.temp.Nov$Rf10.deep.range, k, fill=NA))), { 
  lines(date, range, col=reefcols[3], lwd=2)
})

dev.off()
