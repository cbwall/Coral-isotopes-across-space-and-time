# light atten using DLI
# light attenuation with Depth as FACTOR to show light among depth zones
# this will produce a continuous model curve where light at depth can be calculated
# using the DLI we can then calcualte the integrated light value at each depth where the coral was collected
# at the end, save kd values and use these to convert light at depth in datafile

####################################################
####################################################
### Light attenuation  October deployment ##########
####################################################
####################################################
rm(list=ls())
# using October deployment at 2 depths

df<-read.csv("data/environmental/temp and light/DLI.Oct2016.csv")
df<-df[ , -which(names(df) %in% c("X","Rf44.shall.DLI", "Rf44.mid.DLI"))] # remove R44 w/o 3 levels
df$Date<-as.Date(df$Date)

Oct.DLI<-df
colnames(Oct.DLI)[2:7]<-"DLI" # change all light columns to read "PAR" this is for rbind later


# Adding Depth of logger to dataframe
Rf42.shall<-Oct.DLI[1:2] #only time and PAR
Rf42.shall$Site<-as.factor("Rf42") # make site level
Rf42.shall$Depth<-3*0.3048 # logger at 3 ft, convert to --> meters
Rf42.mid<-Oct.DLI[, c(1,3)]; Rf42.mid$Site<-as.factor("Rf42"); Rf42.mid$Depth<-8*0.3048 # logger at 8 ft 
Rf42.deep<-Oct.DLI[, c(1,4)]; Rf42.deep$Site<-as.factor("Rf42"); Rf42.deep$Depth<-27*0.3048 # logger at 27 ft

HIMB.shall<-Oct.DLI[, c(1,5)]; HIMB.shall$Site<-as.factor("HIMB"); HIMB.shall$Depth<-1*0.3048 # logger at 1 ft
HIMB.mid<-Oct.DLI[, c(1,6)]; HIMB.mid$Site<-as.factor("HIMB"); HIMB.mid$Depth<-6*0.3048 # logger at 6 ft
HIMB.deep<-Oct.DLI[, c(1,7)]; HIMB.deep$Site<-as.factor("HIMB"); HIMB.deep$Depth<-25*0.3048 # logger at 25 ft

# bind together all the data in single dataframe with 4 columns: time, PAR, Site, Depth
DLI.df<-rbind(Rf42.shall, Rf42.mid, Rf42.deep, HIMB.shall, HIMB.mid, HIMB.deep)
DLI.df$log.DLI<-log(DLI.df$DLI) # log transform PAR data
DLI.df<-DLI.df[, c(1,3:4,2,5)]


# Break up into each reef
#####
DLI.Rf42<-DLI.df[(DLI.df$Site=="Rf42"),] # just Rf42
DLI.Rf42$Depth<-as.factor(DLI.Rf42$Depth) # depth as factor
DLI.Rf42$Depth <- factor(DLI.Rf42$Depth, levels = c("2.4384", "0.9144", "8.2296"))
DLI.Rf42$log.DLI<-log(DLI.Rf42$DLI)

mod<-lm(log.DLI~Depth, data=DLI.Rf42, na.action=na.exclude) # Rf42 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf42.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])

shal<-Rf42.coeffs[2]; mid<-Rf42.coeffs[1]; deep<-Rf42.coeffs[3]

####
####
# make new dataframes for Sites
Rf42.df<-df[1:4] 

####
# making some log data
Rf42.df$log.shal<-log(Rf42.df$Rf42.shall.DLI)
Rf42.df$log.mid<-log(Rf42.df$Rf42.mid.DLI)
Rf42.df$log.deep<-log(Rf42.df$Rf42.deep.DLI)

# making some log data using coefficients
Rf42.df$coef.shal<-Rf42.df$log.mid + shal
Rf42.df$coef.mid<-Rf42.df$log.mid
Rf42.df$coef.deep<-Rf42.df$log.mid + deep

##################
##### Figure #####
# how does data comDLIe?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf42.df$log.shal, x=Rf42.df$Date, type="l", col="dodgerblue", 
     main="Reef 42 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="log(DLI)", xlab="", ylim=c(0,8))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$log.mid, x=Rf42.df$Date, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$log.deep, x=Rf42.df$Date, type="l", col="paleturquoise3"))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=Rf42.df$coef.shal, x=Rf42.df$Date, type="l", col="dodgerblue", ylab="log(DLI)", 
     xlab="Date", ylim=c(0,8))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.mid, x=Rf42.df$Date, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.deep, x=Rf42.df$Date, type="l", col="paleturquoise3"))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)


################################ 
########### HIMB ############### 
DLI.HIMB<-DLI.df[(DLI.df$Site=="HIMB"),] # just HIMB
DLI.HIMB$Depth<-as.factor(DLI.HIMB$Depth) # depth as factor
DLI.HIMB$Depth <- factor(DLI.HIMB$Depth, levels = c("1.8288", "0.3048", "7.62"))

mod<-lm(log(DLI)~Depth, data=DLI.HIMB, na.action=na.exclude) # HIMB dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
HIMB.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])
shal<-HIMB.coeffs[2]; mid<-HIMB.coeffs[1]; deep<-HIMB.coeffs[3]

####
####
# make new dataframes for Sites
HIMB.df<-df[, c(1,5:7)]; 
HIMB.df[HIMB.df<=0.01] <- NA # remove leftover values that remain low independently

####
# making some log data
HIMB.df$log.shal<-log(HIMB.df$HIMB.shall.DLI)
HIMB.df$log.mid<-log(HIMB.df$HIMB.mid.DLI)
HIMB.df$log.deep<-log(HIMB.df$HIMB.deep.DLI)

# making some log data using coefficients
HIMB.df$coef.shal<-HIMB.df$log.mid + shal
HIMB.df$coef.mid<-HIMB.df$log.mid 
HIMB.df$coef.deep<-HIMB.df$log.mid + deep


##################
##### Figure #####

# plot log data
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=HIMB.df$log.shal, x=HIMB.df$Date, type="l", col="mediumorchid2", 
     main="HIMB logger (top), 2m-coeff (bottom)", cex.main=0.7, ylab="log(DLI)", 
     xlab="", ylim=c(0,8))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$log.mid, x=HIMB.df$Date, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$log.deep, x=HIMB.df$Date, type="l", col="plum4"))

# plot of modeled 1m and 8m using <2m model coeffs. 
plot(y=HIMB.df$coef.shal, x=HIMB.df$Date, type="l", col="mediumorchid2", 
     cex.main=0.7, ylab="log(DLI)", xlab="", ylim=c(0,8))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.mid, x=HIMB.df$Date, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.deep, x=HIMB.df$Date, type="l", col="plum4"))
legend("top", lty=1, col=c("mediumorchid2", "gray", "plum4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)



####################################################
####################################################
### Light attenuation  November deployment #########
####################################################
####################################################

# using November deployment at 3 depths

df<-read.csv("data/environmental/temp and light/DLI.Nov2016.csv"); df<-df[-1]
df$Date<-as.POSIXct(df$Date)
df[df<=0.01] <- NA # remove low values to get day only

Nov.DLI<-df
colnames(Nov.DLI)[2:7]<-"DLI" # change all light columns to read "DLI" this is for rbind later

Rf44.shall<-Nov.DLI[1:2] #only time and DLI
Rf44.shall$Site<-as.factor("Rf44") # make site level
Rf44.shall$Depth<-1*0.3048 # logger at 1 ft, convert to --> meters
Rf44.mid<-Nov.DLI[, c(1,3)]; Rf44.mid$Site<-as.factor("Rf44"); Rf44.mid$Depth<-6*0.3048 # logger at 6 ft 
Rf44.deep<-Nov.DLI[, c(1,4)]; Rf44.deep$Site<-as.factor("Rf44"); Rf44.deep$Depth<-25*0.3048 # logger at 25 ft

Rf10.shall<-Nov.DLI[, c(1,5)]; Rf10.shall$Site<-as.factor("Rf10"); Rf10.shall$Depth<-2*0.3048 # logger at 2 ft
Rf10.mid<-Nov.DLI[, c(1,6)]; Rf10.mid$Site<-as.factor("Rf10"); Rf10.mid$Depth<-6*0.3048 # logger at 6 ft
Rf10.deep<-Nov.DLI[, c(1,7)]; Rf10.deep$Site<-as.factor("Rf10"); Rf10.deep$Depth<-26*0.3048 # logger at 26 ft

# bind together
DLI.df<-rbind(Rf44.shall, Rf44.mid, Rf44.deep, Rf10.shall, Rf10.mid, Rf10.deep)

# Break up into each reef
#####
DLI.Rf44<-DLI.df[(DLI.df$Site=="Rf44"),] # just Rf44
DLI.Rf44$Depth<-as.factor(DLI.Rf44$Depth) # depth as factor
DLI.Rf44$Depth <- factor(DLI.Rf44$Depth, levels = c("1.8288", "0.3048", "7.62"))
DLI.Rf44$log.DLI<-log(DLI.Rf44$DLI)

mod<-lm(log.DLI~Depth, data=DLI.Rf44, na.action=na.exclude) # R44 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf44.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])
shal<-Rf44.coeffs[2]; mid<-Rf44.coeffs[1]; deep<-Rf44.coeffs[3]

####
####
# make new dataframes for Sites
Rf44.df<-df[1:4] 

####
# making some log data
Rf44.df$log.shal<-log(Rf44.df$Rf44.shall.DLI)
Rf44.df$log.mid<-log(Rf44.df$Rf44.mid.DLI)
Rf44.df$log.deep<-log(Rf44.df$Rf44.deep.DLI)

# making some log data using coefficients
Rf44.df$coef.shal<-Rf44.df$log.mid + shal
Rf44.df$coef.mid<-Rf44.df$log.mid
Rf44.df$coef.deep<-Rf44.df$log.mid + deep

##################
##### Figure #####
# how does data comDLIe?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf44.df$log.shal, x=Rf44.df$Date, type="l", col="yellowgreen", 
     main="Reef 44 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="log(DLI)", xlab="", ylim=c(0,8))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$log.mid, x=Rf44.df$Date, type="l", col="gray"))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$log.deep, x=Rf44.df$Date, type="l", col="palegreen4"))

##### plot of modeled 2m and 8m using <1m model coeffs. 
plot(y=Rf44.df$coef.shal, x=Rf44.df$Date, type="l", col="yellowgreen", ylab="log(DLI)", 
     xlab="", ylim=c(0,8))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.mid, x=Rf44.df$Date, type="l", col="gray"))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.deep, x=Rf44.df$Date, type="l", col="palegreen4"))
legend("top", lty=1, col=c("yellowgreen", "gray", "palegreen4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)



###################################
########### Reef 10 ############### 
DLI.Rf10<-DLI.df[(DLI.df$Site=="Rf10"),] # just Rf10
DLI.Rf10$Depth<-as.factor(DLI.Rf10$Depth) # depth as factor
DLI.Rf10$Depth <- factor(DLI.Rf10$Depth, levels = c("1.8288", "0.6096", "7.9248"))

mod<-lm(log(DLI)~Depth, data=DLI.Rf10, na.action=na.exclude) # Rf10 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf10.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])
shal<-Rf10.coeffs[2]; mid<-Rf10.coeffs[1]; deep<-Rf10.coeffs[3]

####
####
# make new dataframes for Sites
Rf10.df<-df[, c(1,5:7)]

####
# making some log data
Rf10.df$log.shal<-log(Rf10.df$Rf10.shall.DLI)
Rf10.df$log.mid<-log(Rf10.df$Rf10.mid.DLI)
Rf10.df$log.deep<-log(Rf10.df$Rf10.deep.DLI)

# making some log data using coefficients
Rf10.df$coef.shal<-Rf10.df$log.mid + shal
Rf10.df$coef.mid<-Rf10.df$log.mid
Rf10.df$coef.deep<-Rf10.df$log.mid + deep

##################
##### Figure #####

# plot log data
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf10.df$log.shal, x=Rf10.df$Date, type="l", col="orangered", 
     main="Reef 10 logger (top), 2m-coeff (bottom)", cex.main=0.7, ylab="log(DLI)", 
     xlab="", ylim=c(0,8))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$log.mid, x=Rf10.df$Date, type="l", col="gray"))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$log.deep, x=Rf10.df$Date, type="l", col="rosybrown2"))

# plot of modeled 2m and 8m using <1m model coeffs. 
plot(y=Rf10.df$coef.shal, x=Rf10.df$Date, type="l", col="orangered", 
     cex.main=0.7, ylab="log(DLI)", xlab="", ylim=c(0,8))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.mid, x=Rf10.df$Date, type="l", col="gray"))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.deep, x=Rf10.df$Date, type="l", col="rosybrown2"))
legend("top", lty=1, col=c("orangered", "gray", "rosybrown2"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.45), seg.len=1, cex=0.9, xpd=TRUE, horiz=TRUE)


########
# combine all coefficients
all.coefficients<-rbind(Rf42.coeffs, HIMB.coeffs, Rf44.coeffs, Rf10.coeffs)
write.csv(all.coefficients, "data/environmental/DLI factor coeffs.csv")

###########

par(mfrow=c(4,3), mar=c(4,4,2,2))
d<-Rf42.df
plot(d$log.shal~d$coef.shal); abline(lm(d$log.shal~d$coef.shal), col="red")
plot(d$log.mid~d$coef.mid, main="Reef 42"); abline(lm(d$log.mid~d$coef.mid), col="red")
plot(d$log.deep~d$coef.deep); abline(lm(d$log.deep~d$coef.deep), col="red")

d<-Rf44.df
plot(d$log.shal~d$coef.shal); abline(lm(d$log.shal~d$coef.shal), col="red")
plot(d$log.mid~d$coef.mid, main="Reef 44"); abline(lm(d$log.mid~d$coef.mid), col="red")
plot(d$log.deep~d$coef.deep); abline(lm(d$log.deep~d$coef.deep), col="red")

d<-HIMB.df
plot(d$log.shal~d$coef.shal); abline(lm(d$log.shal~d$coef.shal), col="red")
plot(d$log.mid~d$coef.mid, main="HIMB"); abline(lm(d$log.mid~d$coef.mid), col="red")
plot(d$log.deep~d$coef.deep); abline(lm(d$log.deep~d$coef.deep), col="red")

d<-Rf10.df
plot(d$log.shal~d$coef.shal); abline(lm(d$log.shal~d$coef.shal), col="red")
plot(d$log.mid~d$coef.mid, main="Reef 10"); abline(lm(d$log.mid~d$coef.mid), col="red")
plot(d$log.deep~d$coef.deep); abline(lm(d$log.deep~d$coef.deep), col="red")

#dev.copy(pdf, "figures/environmental/DLI_coef_ob.ex.factor.pdf", height=6, width=6)
#dev.off()

########### 


#########################
#########################
#########################

##### 
##### all DLI
files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/all DLI", pattern = "csv$", full.names = T)
tables <- lapply(files, read.csv, header = TRUE)
All.DLI<-do.call(rbind, tables)
All.DLI=All.DLI[-1]
All.DLI$Date<-as.POSIXct(All.DLI$Date) # fix date

# make a date sequence for entire study
all.date.time<-as.data.frame(seq(
  from=as.POSIXct("2016-06-10", tz="HST"),
  to=as.POSIXct("2017-01-12", tz="HST"),
  by="1 d"))  
colnames(all.date.time)[1]<-"Date"

# merge the DLI data and the date sequence to make a complete df through time
DLI.3site<-merge(all.date.time, All.DLI, by="Date", all.x=T)
R10.DLI<-read.csv("data/environmental/temp and light/Jun_DecPAR/all DLI/Reef 10/DLI.Rf102016.csv")
R10.DLI<-R10.DLI[-1]; R10.DLI$Date<-as.POSIXct(R10.DLI$Date)

DLI.4site<-merge(DLI.3site, R10.DLI, by="Date", all.x=T)
DLI.4site$month <- months(as.Date(DLI.4site$Date)) # makes a month column
DLI.4site<-DLI.4site[, c(1,6, 2:5)]

df<-DLI.4site # all the instantaneous light data
df[df<=0.01] <- NA

coeffs<-read.csv("data/environmental/DLI factor coeffs.csv") # modeled coefficients
coeffs$Site<-as.factor(c("Rf42.coeffs", "HIMB.coeffs", "Rf44.coeffs", "R10.coeffs"))
colnames(coeffs)<-c("Site", "Intercept", "shal.coeff", "deep.coeff")

# log transform DLI data to determine light at depth, then reverse transform

## example calcs
# df$log.shal<-df$log.mid - mid.coeff
# df$log.mid<-df$log.mid
# df$log.deep<-df$log.mid - mid.coeff + deep.coeff

df$logRf42.mid<-log(df$Rf42.mid.DLI)
df$logRf42.shall<-log(df$Rf42.mid.DLI) + coeffs[1,3]
df$logRf42.deep<-log(df$Rf42.mid.DLI) + coeffs[1,4]

df$logHIMB.mid<-log(df$HIMB.mid.DLI)
df$logHIMB.shall<-log(df$HIMB.mid.DLI) + coeffs[2,3]
df$logHIMB.deep<-log(df$HIMB.mid.DLI) + coeffs[2,4]

df$logRf44.mid<-log(df$Rf44.mid.DLI)
df$logRf44.shall<-log(df$Rf44.mid.DLI) + coeffs[3,3]
df$logRf44.deep<-log(df$Rf44.mid.DLI) + coeffs[3,4]

df$logRf10.mid<-log(df$Rf10.mid.DLI)
df$logRf10.shall<-log(df$Rf10.mid.DLI) + coeffs[4,3]
df$logRf10.deep<-log(df$Rf10.mid.DLI) + coeffs[4,4]


df$bt.Rf42.mid<-1*exp(df$logRf42.mid)
df$bt.Rf42.shall<-1*exp(df$logRf42.shall)
df$bt.Rf42.deep<-1*exp(df$logRf42.deep)

df$bt.Rf44.mid<-1*exp(df$logRf44.mid)
df$bt.Rf44.shall<-1*exp(df$logRf44.shall)
df$bt.Rf44.deep<-1*exp(df$logRf44.deep)

df$bt.HIMB.mid<-1*exp(df$logHIMB.mid)
df$bt.HIMB.shall<-1*exp(df$logHIMB.shall)
df$bt.HIMB.deep<-1*exp(df$logHIMB.deep)

df$bt.Rf10.mid<-1*exp(df$logRf10.mid)
df$bt.Rf10.shall<-1*exp(df$logRf10.shall)
df$bt.Rf10.deep<-1*exp(df$logRf10.deep)


All.DLI.df<-df[, (names(df) %in% c("Date", "month", 
                                   "bt.Rf42.shall", "bt.Rf42.mid", "bt.Rf42.deep",
                                   "bt.Rf44.shall", "bt.Rf44.mid", "bt.Rf44.deep", 
                                   "bt.HIMB.shall", "bt.HIMB.mid", "bt.HIMB.deep",  
                                   "bt.Rf10.shall", "bt.Rf10.mid", "bt.Rf10.deep"))]

colnames(All.DLI.df)<-c("Date", "month", 
                        "Rf42.mid", "Rf42.shall", "Rf42.deep",
                        "Rf44.mid", "Rf44.shall", "Rf44.deep", 
                        "HIMB.mid", "HIMB.shall", "HIMB.deep",  
                        "Rf10.mid", "Rf10.shall", "Rf10.deep")


# Plot it!!
# Reef 44
par(mfrow=c(2,2), mar=c(4,5,2,2))

plot(y=All.DLI.df$Rf44.shall, x=All.DLI.df$Date, type="l", col="palegreen2", 
     cex.main=0.7, ylab=(expression(paste("DLI"~(mol~photons~m^-2~d^-1), sep="")))
     , xlab="", main="Reef 44", cex.main=1, ylim=c(0, 60))
par(new=T)
with(All.DLI.df, lines(y=All.DLI.df$Rf44.mid, x=All.DLI.df$Date, type="l", col="gray"))
par(new=T)
with(All.DLI.df, lines(y=All.DLI.df$Rf44.deep, x=All.DLI.df$Date, type="l", col="palegreen4"))
legend("topright", lty=1, col=c("palegreen2", "gray", "palegreen4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)


# Plot it!!
# Reef 42
plot(y=All.DLI.df$Rf42.shall, x=All.DLI.df$Date, type="l", col="dodgerblue", 
     cex.main=0.7, ylab=(expression(paste("DLI"~(mol~photons~m^-2~d^-1), sep="")))
     , xlab="", main="Reef 42", cex.main=1, ylim=c(0, 60))
par(new=T)
with(All.DLI.df, lines(y=All.DLI.df$Rf42.mid, x=All.DLI.df$Date, type="l", col="gray"))
par(new=T)
with(All.DLI.df, lines(y=All.DLI.df$Rf42.deep, x=All.DLI.df$Date, type="l", col="skyblue"))
legend("topright", lty=1, col=c("dodgerblue", "gray", "skyblue"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

# Plot it!!
# Reef 10
plot(y=All.DLI.df$Rf10.shall, x=All.DLI.df$Date, type="l", col="orangered", 
     cex.main=0.7, ylab=(expression(paste("DLI"~(mol~photons~m^-2~d^-1), sep="")))
     , xlab="Dates", main="Reef 10", cex.main=1, ylim=c(0, 60))
par(new=T)
with(All.DLI.df, lines(y=All.DLI.df$Rf10.mid, x=All.DLI.df$Date, type="l", col="gray"))
par(new=T)
with(All.DLI.df, lines(y=All.DLI.df$Rf10.deep, x=All.DLI.df$Date, type="l", col="lightsalmon3"))
legend("topright", lty=1, col=c("orangered", "gray", "lightsalmon3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

# Plot it!
# HIMB
plot(y=All.DLI.df$HIMB.shall, x=All.DLI.df$Date, type="l", col="mediumorchid2", 
     cex.main=0.7, ylab=(expression(paste("DLI"~(mol~photons~m^-2~d^-1), sep="")))
     , xlab="Dates", main="HIMB", cex.main=1, ylim=c(0, 60))
par(new=T)
with(All.DLI.df, lines(y=All.DLI.df$HIMB.mid, x=All.DLI.df$Date, type="l", col="gray"))
par(new=T)
with(All.DLI.df, lines(y=All.DLI.df$HIMB.deep, x=All.DLI.df$Date, type="l", col="plum4"))
legend("topright", lty=1, col=c("mediumorchid2", "gray", "plum4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

dev.copy(pdf, "figures/environmental/DLI.all depths.pdf", height=7, width=8)
dev.off()
####
#### end
####
