# light atten using DLI
# light attenuation with Depth as CONTINUOUS to show light among depth zones
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

####
# make new dataframes for Sites
Rf42.df<-df[1:4] 
Rf42.depths<-c(3*0.3048, 8*0.3048, 27*0.3048)

####
# Beer-Lambert Law:   Ez(d) = Ez(0)e^(-k_d *d)  
# where d is depth, k_d is the attenuation coefficient, Ez(d) is the light reaching depth d and Ez(0) is the incident light at the surface.

# with 2m as baseline:  E(depth) = E(2m)*e^(-k_d*(depth - 2m))
# making some log data
Rf42.df$log.shal<-log(Rf42.df$Rf42.shall.DLI)
Rf42.df$log.mid<-log(Rf42.df$Rf42.mid.DLI)
Rf42.df$log.deep<-log(Rf42.df$Rf42.deep.DLI)

Rf42.df$log.delta.mid.deep<-Rf42.df$log.mid-Rf42.df$log.deep # difference in 2m - 8m light
Rf42.df$log.delta.mid.shal<-Rf42.df$log.mid-Rf42.df$log.shal # difference in 2m - 1m light
Rf42.df$dm.depth<-Rf42.depths[3]-Rf42.depths[2] # difference in depth 8m - 2m
Rf42.df$mm.depth<-Rf42.depths[2]-Rf42.depths[2] # difference in depth 2m - 2m
Rf42.df$sm.depth<-Rf42.depths[1]-Rf42.depths[2] # difference in depth 1m - 2m

new.df<-Rf42.df[8:11]; colnames(new.df)<-c("log.DLI.delta", "log.DLI.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
df.test<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

kd.mod<-lm(log.DLI.delta~Depth.delta+0, data=df.test, na.action=na.exclude) # Rf42 dataframe with Depth as a factor
kd<-coef(kd.mod)[1] # use KD here to calculate light at any depth

kd.R42<-kd ### save this

# example
par(mfrow=c(3,1))
Rf42.df$Rf42.expect<-Rf42.df$Rf42.mid.DLI*exp(-kd*Rf42.df$sm.depth)
plot(Rf42.df$Rf42.expect~Rf42.df$Rf42.shall.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="SHALLOW observed DLI")
abline(lm(Rf42.df$Rf42.expect~Rf42.df$Rf42.shall.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

Rf42.df$Rf42.expect<-Rf42.df$Rf42.mid.DLI*exp(-kd*Rf42.df$mm.depth)
plot(Rf42.df$Rf42.expect~Rf42.df$Rf42.mid.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="MID DEPTH observed DLI")
abline(lm(Rf42.df$Rf42.expect~Rf42.df$Rf42.mid.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

Rf42.df$Rf42.expect<-Rf42.df$Rf42.mid.DLI*exp(-kd*Rf42.df$dm.depth)
plot(Rf42.df$Rf42.expect~Rf42.df$Rf42.deep.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="DEEP observed DLI")
abline(lm(Rf42.df$Rf42.expect~Rf42.df$Rf42.deep.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.3)


# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
Rf42.df$coef.shal<-Rf42.df$Rf42.mid.DLI*exp(-kd*Rf42.df$sm.depth)
Rf42.df$coef.mid<-Rf42.df$Rf42.mid.DLI*exp(-kd*Rf42.df$mm.depth)
Rf42.df$coef.deep<-Rf42.df$Rf42.mid.DLI*exp(-kd*Rf42.df$dm.depth)

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf42.df$Rf42.shall.DLI, x=Rf42.df$Date, type="l", col="dodgerblue", 
     main="Reef 42 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="DLI", xlab="", ylim=c(0,40), lwd=2)
par(new=T)
with(Rf42.df, lines(y=Rf42.df$Rf42.mid.DLI, x=Rf42.df$Date, type="l", col="gray", lwd=2))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$Rf42.deep.DLI, x=Rf42.df$Date, type="l", col="paleturquoise3", lwd=2))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=Rf42.df$coef.shal, x=Rf42.df$Date, type="l", col="dodgerblue", ylab="log(PAR)", 
     xlab="Date", ylim=c(0,40), lwd=2)
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.mid, x=Rf42.df$Date, type="l", col="gray", lwd=2))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.deep, x=Rf42.df$Date, type="l", col="paleturquoise3", lwd=2))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/DLI_coef_Rf42.pdf", height=6, width=4)
dev.off()


####################################
###################################

############ HIMB ############
# make new dataframes for Sites
HIMB.df<-df[, c(1,5:7)] 
HIMB.depths<-c(1*0.3048, 6*0.3048, 25*0.3048)

####
# with 2m as baseline:  E(depth) = E(2m)*e^(-k_d*(depth - 2m))
# making some log data
HIMB.df$log.shal<-log(HIMB.df$HIMB.shall.DLI)
HIMB.df$log.mid<-log(HIMB.df$HIMB.mid.DLI)
HIMB.df$log.deep<-log(HIMB.df$HIMB.deep.DLI)

HIMB.df$log.delta.mid.deep<-HIMB.df$log.mid-HIMB.df$log.deep # difference in 2m - 8m light
HIMB.df$log.delta.mid.shal<-HIMB.df$log.mid-HIMB.df$log.shal # difference in 2m - 1m light
HIMB.df$dm.depth<-HIMB.depths[3]-HIMB.depths[2] # difference in depth 8m - 2m
HIMB.df$mm.depth<-HIMB.depths[2]-HIMB.depths[2] # difference in depth 2m - 2m
HIMB.df$sm.depth<-HIMB.depths[1]-HIMB.depths[2] # difference in depth 1m - 2m

new.df<-HIMB.df[8:11]; colnames(new.df)<-c("log.DLI.delta", "log.DLI.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
df.test<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

kd.mod<-lm(log.DLI.delta~Depth.delta+0, data=df.test, na.action=na.exclude) # HIMB dataframe with Depth as a factor
kd<-coef(kd.mod)[1] # use KD here to calculate light at any depth

kd.HIMB<-kd ### save this

# example
par(mfrow=c(3,1))
HIMB.df$HIMB.expect<-HIMB.df$HIMB.mid.DLI*exp(-kd*HIMB.df$sm.depth)
plot(HIMB.df$HIMB.expect~HIMB.df$HIMB.shall.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="SHALLOW observed DLI")
abline(lm(HIMB.df$HIMB.expect~HIMB.df$HIMB.shall.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

HIMB.df$HIMB.expect<-HIMB.df$HIMB.mid.DLI*exp(-kd*HIMB.df$mm.depth)
plot(HIMB.df$HIMB.expect~HIMB.df$HIMB.mid.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="MID DEPTH observed DLI")
abline(lm(HIMB.df$HIMB.expect~HIMB.df$HIMB.mid.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

HIMB.df$HIMB.expect<-HIMB.df$HIMB.mid.DLI*exp(-kd*HIMB.df$dm.depth)
plot(HIMB.df$HIMB.expect~HIMB.df$HIMB.deep.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="DEEP observed DLI")
abline(lm(HIMB.df$HIMB.expect~HIMB.df$HIMB.deep.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.3)


# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
HIMB.df$coef.shal<-HIMB.df$HIMB.mid.DLI*exp(-kd*HIMB.df$sm.depth)
HIMB.df$coef.mid<-HIMB.df$HIMB.mid.DLI*exp(-kd*HIMB.df$mm.depth)
HIMB.df$coef.deep<-HIMB.df$HIMB.mid.DLI*exp(-kd*HIMB.df$dm.depth)

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=HIMB.df$HIMB.shall.DLI, x=HIMB.df$Date, type="l", col="dodgerblue", 
     main="HIMB logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="DLI", xlab="", ylim=c(0,40), lwd=2)
par(new=T)
with(HIMB.df, lines(y=HIMB.df$HIMB.mid.DLI, x=HIMB.df$Date, type="l", col="gray", lwd=2))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$HIMB.deep.DLI, x=HIMB.df$Date, type="l", col="paleturquoise3", lwd=2))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=HIMB.df$coef.shal, x=HIMB.df$Date, type="l", col="dodgerblue", ylab="log(PAR)", 
     xlab="Date", ylim=c(0,40), lwd=2)
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.mid, x=HIMB.df$Date, type="l", col="gray", lwd=2))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.deep, x=HIMB.df$Date, type="l", col="paleturquoise3", lwd=2))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/DLI_coef_HIMB.pdf", height=6, width=4)
dev.off()



####################################################
####################################################
### Light attenuation  November deployment #########
####################################################
####################################################

# using November deployment at 3 depths

df<-read.csv("data/environmental/temp and light/DLI.Nov2016.csv")
df<-df[ , -which(names(df) %in% "X")] # remove "X"
df$Date<-as.Date(df$Date)

Nov.DLI<-df
colnames(Nov.DLI)[2:7]<-"DLI" # change all light columns to read "PAR" this is for rbind later

# Adding Depth of logger to dataframe
Rf44.shall<-Nov.DLI[1:2] #only time and PAR
Rf44.shall$Site<-as.factor("Rf44") # make site level
Rf44.shall$Depth<-1*0.3048 # logger at 1 ft, convert to --> meters
Rf44.mid<-Nov.DLI[, c(1,3)]; Rf44.mid$Site<-as.factor("Rf44"); Rf44.mid$Depth<-6*0.3048 # logger at 6 ft 
Rf44.deep<-Nov.DLI[, c(1,4)]; Rf44.deep$Site<-as.factor("Rf44"); Rf44.deep$Depth<-25*0.3048 # logger at 25 ft

Rf10.shall<-Nov.DLI[, c(1,5)]; Rf10.shall$Site<-as.factor("Rf10"); Rf10.shall$Depth<-2*0.3048 # logger at 2 ft
Rf10.mid<-Nov.DLI[, c(1,6)]; Rf10.mid$Site<-as.factor("Rf10"); Rf10.mid$Depth<-6*0.3048 # logger at 6 ft
Rf10.deep<-Nov.DLI[, c(1,7)]; Rf10.deep$Site<-as.factor("Rf10"); Rf10.deep$Depth<-26*0.3048 # logger at 26 ft

# bind together all the data in single dataframe with 4 columns: time, PAR, Site, Depth
DLI.df<-rbind(Rf44.shall, Rf44.mid, Rf44.deep, Rf10.shall, Rf10.mid, Rf10.deep)
DLI.df$log.DLI<-log(DLI.df$DLI) # log transform PAR data
DLI.df<-DLI.df[, c(1,3:4,2,5)]

###############
###############

# make new dataframes for Sites
Rf44.df<-df[1:4] 
Rf44.depths<-c(1*0.3048, 6*0.3048, 25*0.3048)

####
# with 2m as baseline:  E(depth) = E(2m)*e^(-k_d*(depth - 2m))
# making some log data
Rf44.df$log.shal<-log(Rf44.df$Rf44.shall.DLI)
Rf44.df$log.mid<-log(Rf44.df$Rf44.mid.DLI)
Rf44.df$log.deep<-log(Rf44.df$Rf44.deep.DLI)

Rf44.df$log.delta.mid.deep<-Rf44.df$log.mid-Rf44.df$log.deep # difference in 2m - 8m light
Rf44.df$log.delta.mid.shal<-Rf44.df$log.mid-Rf44.df$log.shal # difference in 2m - 1m light
Rf44.df$dm.depth<-Rf44.depths[3]-Rf44.depths[2] # difference in depth 8m - 2m
Rf44.df$mm.depth<-Rf44.depths[2]-Rf44.depths[2] # difference in depth 2m - 2m
Rf44.df$sm.depth<-Rf44.depths[1]-Rf44.depths[2] # difference in depth 1m - 2m

new.df<-Rf44.df[8:11]; colnames(new.df)<-c("log.DLI.delta", "log.DLI.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
df.test<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

kd.mod<-lm(log.DLI.delta~Depth.delta+0, data=df.test, na.action=na.exclude) # Rf44 dataframe with Depth as a factor
kd<-coef(kd.mod)[1] # use KD here to calculate light at any depth

kd.R44<-kd ### save this

# example
par(mfrow=c(3,1))
Rf44.df$Rf44.expect<-Rf44.df$Rf44.mid.DLI*exp(-kd*Rf44.df$sm.depth)
plot(Rf44.df$Rf44.expect~Rf44.df$Rf44.shall.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="SHALLOW observed DLI")
abline(lm(Rf44.df$Rf44.expect~Rf44.df$Rf44.shall.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

Rf44.df$Rf44.expect<-Rf44.df$Rf44.mid.DLI*exp(-kd*Rf44.df$mm.depth)
plot(Rf44.df$Rf44.expect~Rf44.df$Rf44.mid.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="MID DEPTH observed DLI")
abline(lm(Rf44.df$Rf44.expect~Rf44.df$Rf44.mid.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

Rf44.df$Rf44.expect<-Rf44.df$Rf44.mid.DLI*exp(-kd*Rf44.df$dm.depth)
plot(Rf44.df$Rf44.expect~Rf44.df$Rf44.deep.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="DEEP observed DLI")
abline(lm(Rf44.df$Rf44.expect~Rf44.df$Rf44.deep.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.3)


# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
Rf44.df$coef.shal<-Rf44.df$Rf44.mid.DLI*exp(-kd*Rf44.df$sm.depth)
Rf44.df$coef.mid<-Rf44.df$Rf44.mid.DLI*exp(-kd*Rf44.df$mm.depth)
Rf44.df$coef.deep<-Rf44.df$Rf44.mid.DLI*exp(-kd*Rf44.df$dm.depth)

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf44.df$Rf44.shall.DLI, x=Rf44.df$Date, type="l", col="dodgerblue", 
     main="Reef 44 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="DLI", xlab="", ylim=c(0,40), lwd=2)
par(new=T)
with(Rf44.df, lines(y=Rf44.df$Rf44.mid.DLI, x=Rf44.df$Date, type="l", col="gray", lwd=2))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$Rf44.deep.DLI, x=Rf44.df$Date, type="l", col="paleturquoise3", lwd=2))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=Rf44.df$coef.shal, x=Rf44.df$Date, type="l", col="dodgerblue", ylab="log(PAR)", 
     xlab="Date", ylim=c(0,40), lwd=2)
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.mid, x=Rf44.df$Date, type="l", col="gray", lwd=2))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.deep, x=Rf44.df$Date, type="l", col="paleturquoise3", lwd=2))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/DLI_coef_Rf44.pdf", height=6, width=4)
dev.off()


################
################

## Reef 10 ############

# make new dataframes for Sites
Rf10.df<-df[, c(1,5:7)] 
Rf10.depths<-c(2*0.3048, 6*0.3048, 26*0.3048)

####
# with 2m as baseline:  E(depth) = E(2m)*e^(-k_d*(depth - 2m))
# making some log data
Rf10.df$log.shal<-log(Rf10.df$Rf10.shall.DLI)
Rf10.df$log.mid<-log(Rf10.df$Rf10.mid.DLI)
Rf10.df$log.deep<-log(Rf10.df$Rf10.deep.DLI)

Rf10.df$log.delta.mid.deep<-Rf10.df$log.mid-Rf10.df$log.deep # difference in 2m - 8m light
Rf10.df$log.delta.mid.shal<-Rf10.df$log.mid-Rf10.df$log.shal # difference in 2m - 1m light
Rf10.df$dm.depth<-Rf10.depths[3]-Rf10.depths[2] # difference in depth 8m - 2m
Rf10.df$mm.depth<-Rf10.depths[2]-Rf10.depths[2] # difference in depth 2m - 2m
Rf10.df$sm.depth<-Rf10.depths[1]-Rf10.depths[2] # difference in depth 1m - 2m

new.df<-Rf10.df[8:11]; colnames(new.df)<-c("log.DLI.delta", "log.DLI.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
df.test<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

kd.mod<-lm(log.DLI.delta~Depth.delta+0, data=df.test, na.action=na.exclude) # Rf10 dataframe with Depth as a factor
kd<-coef(kd.mod)[1] # use KD here to calculate light at any depth

kd.R10<-kd ### save this

# example
par(mfrow=c(3,1))
Rf10.df$Rf10.expect<-Rf10.df$Rf10.mid.DLI*exp(-kd*Rf10.df$sm.depth)
plot(Rf10.df$Rf10.expect~Rf10.df$Rf10.shall.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="SHALLOW observed DLI")
abline(lm(Rf10.df$Rf10.expect~Rf10.df$Rf10.shall.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

Rf10.df$Rf10.expect<-Rf10.df$Rf10.mid.DLI*exp(-kd*Rf10.df$mm.depth)
plot(Rf10.df$Rf10.expect~Rf10.df$Rf10.mid.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="MID DEPTH observed DLI")
abline(lm(Rf10.df$Rf10.expect~Rf10.df$Rf10.mid.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.5)

Rf10.df$Rf10.expect<-Rf10.df$Rf10.mid.DLI*exp(-kd*Rf10.df$dm.depth)
plot(Rf10.df$Rf10.expect~Rf10.df$Rf10.deep.DLI, main="lm(log.DLI.delta~Depth.delta + 0)", 
     ylab="expected DLI", xlab="DEEP observed DLI")
abline(lm(Rf10.df$Rf10.expect~Rf10.df$Rf10.deep.DLI), col="red"); abline(0, 1, col="green")
legend("topleft", lty=1, col=c("red", "green"), legend=c("model", "1:1 line"), 
       lwd=1, bty="n", seg.len=1, cex=1.1, y.intersp=0.3)


# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
Rf10.df$coef.shal<-Rf10.df$Rf10.mid.DLI*exp(-kd*Rf10.df$sm.depth)
Rf10.df$coef.mid<-Rf10.df$Rf10.mid.DLI*exp(-kd*Rf10.df$mm.depth)
Rf10.df$coef.deep<-Rf10.df$Rf10.mid.DLI*exp(-kd*Rf10.df$dm.depth)

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf10.df$Rf10.shall.DLI, x=Rf10.df$Date, type="l", col="dodgerblue", 
     main="Reef 10 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="DLI", xlab="", ylim=c(0,40), lwd=2)
par(new=T)
with(Rf10.df, lines(y=Rf10.df$Rf10.mid.DLI, x=Rf10.df$Date, type="l", col="gray", lwd=2))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$Rf10.deep.DLI, x=Rf10.df$Date, type="l", col="paleturquoise3", lwd=2))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=Rf10.df$coef.shal, x=Rf10.df$Date, type="l", col="dodgerblue", ylab="log(PAR)", 
     xlab="Date", ylim=c(0,40), lwd=2)
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.mid, x=Rf10.df$Date, type="l", col="gray", lwd=2))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.deep, x=Rf10.df$Date, type="l", col="paleturquoise3", lwd=2))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/DLI_coef_Rf10.pdf", height=6, width=4)
dev.off()

kd.all<-as.data.frame(c(kd.R42, kd.R44, kd.HIMB, kd.R10))
kd.all$Sites<-as.factor(c("Rf42", "Rf44", "HIMB", "Rf10")); kd.all<-kd.all[2:1]
colnames(kd.all)<-c("Site", "kd")

write.csv(kd.all, "data/environmental/kd.all.csv")
