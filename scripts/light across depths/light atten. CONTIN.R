# light attenuation with Depth as CONTINUOUS to show light among depth zones


####################################################
####################################################
### Light attenuation  October deployment ##########
####################################################
####################################################
rm(list=ls())
# using October deployment at 2 depths

df<-read.csv("data/environmental/temp and light/Field.allPAR.Oct2016.csv")
df<-df[ , -which(names(df) %in% c("X","Rf44.shallow", "Rf44.mid"))] # remove R44 w/o 3 levels
df$timestamp<-as.POSIXct(df$timestamp)
df[df<=1] <- NA # remove low values to get day only

Oct.PAR<-df
colnames(Oct.PAR)[2:7]<-"PAR" # change all light columns to read "PAR" this is for rbind later


# Adding Depth of logger to dataframe
Rf42.shall<-Oct.PAR[1:2] #only time and PAR
Rf42.shall$Site<-as.factor("Rf42") # make site level
Rf42.shall$Depth<-3*0.3048 # logger at 3 ft, convert to --> meters
Rf42.mid<-Oct.PAR[, c(1,3)]; Rf42.mid$Site<-as.factor("Rf42"); Rf42.mid$Depth<-8*0.3048 # logger at 8 ft 
Rf42.deep<-Oct.PAR[, c(1,4)]; Rf42.deep$Site<-as.factor("Rf42"); Rf42.deep$Depth<-27*0.3048 # logger at 27 ft
HIMB.shall<-Oct.PAR[, c(1,5)]; HIMB.shall$Site<-as.factor("HIMB"); HIMB.shall$Depth<-1*0.3048 # logger at 1 ft
HIMB.mid<-Oct.PAR[, c(1,6)]; HIMB.mid$Site<-as.factor("HIMB"); HIMB.mid$Depth<-6*0.3048 # logger at 6 ft
HIMB.deep<-Oct.PAR[, c(1,7)]; HIMB.deep$Site<-as.factor("HIMB"); HIMB.deep$Depth<-25*0.3048 # logger at 25 ft

# bind together all the data in single dataframe with 4 columns: time, PAR, Site, Depth
PAR.df<-rbind(Rf42.shall, Rf42.mid, Rf42.deep, HIMB.shall, HIMB.mid, HIMB.deep)
PAR.df$log.PAR<-log(PAR.df$PAR) # log transform PAR data
PAR.df<-PAR.df[, c(1,3:4,2,5)]

####
# make new dataframes for Sites
Rf42.df<-df[1:4] 
Rf42.depths<-c(3*0.3048, 8*0.3048, 27*0.3048)

####
# Beer-Lambert Law:   Ez(d) = Ez(0)e^(-k_d *d)  
# where d is depth, k_d is the attenuation coefficient, Ez(d) is the light reaching depth d and Ez(0) is the incident light at the surface.

# with 2m as baseline:  E(depth) = E(2m)*e^(-k_d*(depth - 2m))
# making some log data
Rf42.df$log.shal<-log(Rf42.df$Rf42.shallow)
Rf42.df$log.mid<-log(Rf42.df$Rf42.mid)
Rf42.df$log.deep<-log(Rf42.df$Rf42.deep)

Rf42.df$log.delta.mid.deep<-Rf42.df$log.mid-Rf42.df$log.deep # difference in 2m - 8m light
Rf42.df$log.delta.mid.shal<-Rf42.df$log.mid-Rf42.df$log.shal # difference in 2m - 1m light
Rf42.df$dm.depth<-Rf42.depths[3]-Rf42.depths[2] # difference in depth 8m - 2m
Rf42.df$mm.depth<-Rf42.depths[2]-Rf42.depths[2] # difference in depth 2m - 2m
Rf42.df$sm.depth<-Rf42.depths[1]-Rf42.depths[2] # difference in depth 1m - 2m

new.df<-Rf42.df[8:11]; colnames(new.df)<-c("log.PAR.delta", "log.PAR.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
df.test<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

kd.mod<-lm(log.PAR.delta~Depth.delta+0, data=df.test, na.action=na.exclude) # Rf42 dataframe with Depth as a factor
kd<-coef(kd.mod)[1] # use KD here to calculate light at any depth?

# example
par(mfrow=c(3,1))
Rf42.df$Rf42.shallow2<-Rf42.df$Rf42.mid*exp(-kd*Rf42.df$sm.depth)
plot(Rf42.df$Rf42.shallow2~Rf42.df$Rf42.shallow, main="lm(log.PAR.delta~Depth.delta + 0)", 
     ylab="expected PAR", xlab="SHALLOW observed PAR")
abline(lm(Rf42.df$Rf42.shallow2~Rf42.df$Rf42.shallow), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

Rf42.df$Rf42.mid2<-Rf42.df$Rf42.mid*exp(-kd*Rf42.df$mm.depth)
plot(Rf42.df$Rf42.mid2~Rf42.df$Rf42.mid, main="lm(log.PAR.delta~Depth.delta + 0)", 
     ylab="expected PAR", xlab="MID DEPTH observed PAR")
abline(lm(Rf42.df$Rf42.mid2~Rf42.df$Rf42.mid), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

Rf42.df$Rf42.deep2<-Rf42.df$Rf42.mid*exp(-kd*Rf42.df$dm.depth)
plot(Rf42.df$Rf42.deep2~Rf42.df$Rf42.deep, main="lm(log.PAR.delta~Depth.delta + 0)", 
     ylab="expected PAR", xlab="DEEP observed PAR")
abline(lm(Rf42.df$Rf42.deep2~Rf42.df$Rf42.deep), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.3)


# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
Rf42.df$coef.shal<-Rf42.df$Rf42.mid*exp(-kd*Rf42.df$sm.depth)
Rf42.df$coef.mid<-Rf42.df$Rf42.mid*exp(-kd*Rf42.df$mm.depth)
Rf42.df$coef.deep<-Rf42.df$Rf42.mid*exp(-kd*Rf42.df$dm.depth)

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf42.df$Rf42.shallow, x=Rf42.df$timestamp, type="l", col="dodgerblue", 
     main="Reef 42 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="PAR", xlab="", ylim=c(0,1500))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$Rf42.mid, x=Rf42.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$Rf42.deep, x=Rf42.df$timestamp, type="l", col="paleturquoise3"))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=Rf42.df$coef.shal, x=Rf42.df$timestamp, type="l", col="dodgerblue", ylab="PAR", 
     xlab="Date", ylim=c(0,1500))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.mid, x=Rf42.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.deep, x=Rf42.df$timestamp, type="l", col="paleturquoise3"))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_coef_Rf42.pdf", height=6, width=4)
dev.off()


################################ 
########### HIMB ############### 
PAR.HIMB<-PAR.df[(PAR.df$Site=="HIMB"),] # just HIMB
PAR.HIMB$Depth<-as.numeric(PAR.HIMB$Depth) # depth as CONTINUOUS

mod<-lm(log(PAR)~Depth+0, data=PAR.HIMB, na.action=na.exclude) # HIMB dataframe with Depth as a factor
HIMB.coef<-coef(mod)[1]

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

# HIMB Depths
HIMB.depths<-c(1*0.3048, 6*0.3048, 25*0.3048)

# making some log data using coefficients
HIMB.df$coef.shal<-HIMB.df$log.mid*exp(-HIMB.coef*(HIMB.depths[1]))
HIMB.df$coef.mid<-HIMB.df$log.mid*exp(-HIMB.coef*(HIMB.depths[2]))
HIMB.df$coef.deep<-HIMB.df$log.mid*exp(-HIMB.coef*(HIMB.depths[3]))


##################
##### Figure #####

# plot log data
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=HIMB.df$log.shal, x=HIMB.df$timestamp, type="l", col="mediumorchid2", 
     main="HIMB logger (top), 2m-coeff (bottom)", cex.main=0.7, ylab="log(PAR)", 
     xlab="", ylim=c(0,8))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$log.mid, x=HIMB.df$timestamp, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$log.deep, x=HIMB.df$timestamp, type="l", col="plum4"))

# plot of modeled 1m and 8m using <2m model coeffs. 
plot(y=HIMB.df$coef.shal, x=HIMB.df$timestamp, type="l", col="mediumorchid2", 
     cex.main=0.7, ylab="log(PAR)", xlab="", ylim=c(0,8))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.mid, x=HIMB.df$timestamp, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.deep, x=HIMB.df$timestamp, type="l", col="plum4"))
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
PAR.Rf44$Depth<-as.numeric(PAR.Rf44$Depth) # depth as factor
PAR.Rf44$log.PAR<-log(PAR.Rf44$PAR)

mod<-lm(log.PAR~Depth+0, data=PAR.Rf44, na.action=na.exclude) # R44 dataframe with Depth as a factor
Rf44.coeff<-coef(mod)[1]

####
####
# make new dataframes for Sites
Rf44.df<-df[1:4] 

####
# making some log data
Rf44.df$log.shal<-log(Rf44.df$Rf44.shallow)
Rf44.df$log.mid<-log(Rf44.df$Rf44.mid)
Rf44.df$log.deep<-log(Rf44.df$Rf44.deep)

# making some log data using coefficients
Rf44.df$coef.shal<-Rf44.df$log.mid + shal
Rf44.df$coef.mid<-Rf44.df$log.mid
Rf44.df$coef.deep<-Rf44.df$log.mid + deep

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf44.df$log.shal, x=Rf44.df$timestamp, type="l", col="yellowgreen", 
     main="Reef 44 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
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
legend("top", lty=1, col=c("yellowgreen", "gray", "palegreen4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_data_2xcoef_Rf44.pdf", height=4, width=6)
dev.off()


###################################
########### Reef 10 ############### 
PAR.Rf10<-PAR.df[(PAR.df$Site=="Rf10"),] # just Rf10
PAR.Rf10$Depth<-as.numeric(PAR.Rf10$Depth) # depth as factor

mod<-lm(log(PAR)~Depth+0, data=PAR.Rf10, na.action=na.exclude) # Rf10 dataframe with Depth as a factor
Rf10.coeff<-coef(mod)[1]

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
Rf10.df$coef.shal<-Rf10.df$log.mid + shal
Rf10.df$coef.mid<-Rf10.df$log.mid
Rf10.df$coef.deep<-Rf10.df$log.mid + deep

##################
##### Figure #####

# plot log data
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf10.df$log.shal, x=Rf10.df$timestamp, type="l", col="orangered", 
     main="Reef 10 logger (top), 2m-coeff (bottom)", cex.main=0.7, ylab="log(PAR)", 
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
legend("top", lty=1, col=c("orangered", "gray", "rosybrown2"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.45), seg.len=1, cex=0.9, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_data_2xcoef_Rf10.pdf", height=4, width=6)
dev.off()

########
########
# combine all coefficients
all.coefficients<-rbind(Rf42.coeff, HIMB.coeff, Rf44.coeff, Rf10.coeff)
write.csv(all.coefficients, "data/environmental/light coeff_cont.csv")

###########
###########
########### 


#########################
#########################
#########################

##### 
##### all PAR
files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/all PAR", pattern = "csv$", full.names = T)
tables <- lapply(files, read.csv, header = TRUE)
All.PAR<-do.call(rbind, tables)
All.PAR=All.PAR[-1]
All.PAR$timestamp<-as.POSIXct(All.PAR$timestamp) # fix date

# make a date sequence for entire study
all.date.time<-as.data.frame(seq(
  from=as.POSIXct("2016-06-10 00:00:00", tz="HST"),
  to=as.POSIXct("2017-01-12 00:00:00", tz="HST"),
  by="15 min"))  
colnames(all.date.time)[1]<-"timestamp"

# merge the PAR data and the date sequence to make a complete df through time
PAR.3site<-merge(all.date.time, All.PAR, by="timestamp", all.x=T)
R10.par<-read.csv("data/environmental/temp and light/Jun_DecPAR/all PAR/Reef 10/Rf10.PAR.csv")
R10.par<-R10.par[-1]; R10.par$timestamp<-as.POSIXct(R10.par$timestamp)

PAR.4site<-merge(PAR.3site, R10.par, by="timestamp", all.x=T)
PAR.4site$month <- months(as.Date(PAR.4site$timestamp)) # makes a month column
PAR.4site<-PAR.4site[, c(1,6, 2:5)]

# determine monthly mean for PAR during deployments at depth
write.csv(PAR.4site, "data/environmental/temp and light/Jun_DecPAR/All.PAR.csv")


###########
###########
########### 
# read in all PAR data at 2m and model what the 1m and 8m data would look like
df<-PAR.4site # all the instantaneous light data
df[df<=1] <- NA

coeffs<-read.csv("data/environmental/light coeffs.csv") # modeled coefficients
colnames(coeffs)<-c("Site", "Intercept", "shal.coeff", "deep.coeff")

# log transform PAR data to determine light at depth, then reverse transform

## example calcs
# df$log.shal<-df$log.mid - mid.coeff
# df$log.mid<-df$log.mid
# df$log.deep<-df$log.mid - mid.coeff + deep.coeff

df$logRf42.mid<-log(df$Rf42.mid)
df$logRf42.shall<-log(df$Rf42.mid) + coeffs[1,3]
df$logRf42.deep<-log(df$Rf42.mid) + coeffs[1,4]

df$logHIMB.mid<-log(df$HIMB.mid)
df$logHIMB.shall<-log(df$HIMB.mid) + coeffs[2,3]
df$logHIMB.deep<-log(df$HIMB.mid) + coeffs[2,4]

df$logRf44.mid<-log(df$Rf44.mid)
df$logRf44.shall<-log(df$Rf44.mid) + coeffs[3,3]
df$logRf44.deep<-log(df$Rf44.mid) + coeffs[3,4]

df$logRf10.mid<-log(df$Rf10.mid)
df$logRf10.shall<-log(df$Rf10.mid) + coeffs[4,3]
df$logRf10.deep<-log(df$Rf10.mid) + coeffs[4,4]


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


All.PAR.df<-df[, (names(df) %in% c("timestamp", "month", 
                                   "bt.Rf42.shall", "bt.Rf42.mid", "bt.Rf42.deep",
                                   "bt.Rf44.shall", "bt.Rf44.mid", "bt.Rf44.deep", 
                                   "bt.HIMB.shall", "bt.HIMB.mid", "bt.HIMB.deep",  
                                   "bt.Rf10.shall", "bt.Rf10.mid", "bt.Rf10.deep"))]

colnames(All.PAR.df)<-c("timestamp", "month", 
                        "Rf42.mid", "Rf42.shall", "Rf42.deep",
                        "Rf44.mid", "Rf44.shall", "Rf44.deep", 
                        "HIMB.mid", "HIMB.shall", "HIMB.deep",  
                        "Rf10.mid", "Rf10.shall", "Rf10.deep")


# Plot it!!
# Reef 44
par(mfrow=c(2,2), mar=c(4,5,2,2))

plot(y=All.PAR.df$Rf44.shall, x=All.PAR.df$timestamp, type="l", col="palegreen2", 
     cex.main=0.7, ylab=(expression(paste("PAR"~(mu*mol~photons~m^-2~s^-1), sep="")))
     , xlab="", main="Reef 44", cex.main=1, ylim=c(0, 2400))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf44.mid, x=All.PAR.df$timestamp, type="l", col="gray"))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf44.deep, x=All.PAR.df$timestamp, type="l", col="palegreen4"))
legend("topright", lty=1, col=c("palegreen2", "gray", "palegreen4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.25, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)


# Plot it!!
# Reef 42
plot(y=All.PAR.df$Rf42.shall, x=All.PAR.df$timestamp, type="l", col="dodgerblue", 
     cex.main=0.7, ylab=(expression(paste("PAR"~(mu*mol~photons~m^-2~s^-1), sep="")))
     , xlab="", main="Reef 42", cex.main=1, ylim=c(0, 2400))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf42.mid, x=All.PAR.df$timestamp, type="l", col="gray"))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf42.deep, x=All.PAR.df$timestamp, type="l", col="paleturquoise3"))
legend("topright", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.25, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

# Plot it!!
# Reef 10
plot(y=All.PAR.df$Rf10.shall, x=All.PAR.df$timestamp, type="l", col="orangered", 
     cex.main=0.7, ylab=(expression(paste("PAR"~(mu*mol~photons~m^-2~s^-1), sep="")))
     , xlab="Dates", main="Reef 10", cex.main=1, ylim=c(0, 2400))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf10.mid, x=All.PAR.df$timestamp, type="l", col="gray"))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf10.deep, x=All.PAR.df$timestamp, type="l", col="sandybrown"))
legend("topright", lty=1, col=c("orangered", "gray", "sandybrown"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.25, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

# Plot it!
# HIMB
plot(y=All.PAR.df$HIMB.shall, x=All.PAR.df$timestamp, type="l", col="mediumorchid2", 
     cex.main=0.7, ylab=(expression(paste("PAR"~(mu*mol~photons~m^-2~s^-1), sep="")))
     , xlab="Dates", main="HIMB", cex.main=1, ylim=c(0, 2400))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$HIMB.mid, x=All.PAR.df$timestamp, type="l", col="gray"))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$HIMB.deep, x=All.PAR.df$timestamp, type="l", col="plum4"))
legend("topright", lty=1, col=c("mediumorchid2", "gray", "rosybrown2"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.25, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

dev.copy(pdf, "figures/environmental/PAR.all depths.pdf", height=7, width=8)
dev.off()
