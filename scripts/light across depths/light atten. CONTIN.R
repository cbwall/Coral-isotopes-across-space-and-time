# light attenuation with Depth CONTINUOUS to show light among depth zones


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
Rf42.df$sm.depth<-Rf42.depths[1]-Rf42.depths[2] # difference in depth 1m - 2m
Rf42.df$mm.depth<-Rf42.depths[2]-Rf42.depths[2] # difference in depth 2m - 2m

new.df<-Rf42.df[8:11]; colnames(new.df)<-c("log.PAR.delta", "log.PAR.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
PAR.Rf42<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

Rf42.mod<-lm(log.PAR.delta~Depth.delta+0, data=PAR.Rf42, na.action=na.exclude) # Rf42 dataframe, depth continuous
Rf42.coef<-coef(Rf42.mod)[1] # use KD here to calculate light at any depth

# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
Rf42.df$coef.shal<-Rf42.df$Rf42.mid*exp(-Rf42.coef*Rf42.df$sm.depth)
Rf42.df$coef.mid<-Rf42.df$Rf42.mid*exp(-Rf42.coef*Rf42.df$mm.depth)
Rf42.df$coef.deep<-Rf42.df$Rf42.mid*exp(-Rf42.coef*Rf42.df$dm.depth)

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
# with 2m as baseline:  E(depth) = E(2m)*e^(-k_d*(depth - 2m))
# making some log data
HIMB.df$log.shal<-log(HIMB.df$HIMB.shallow)
HIMB.df$log.mid<-log(HIMB.df$HIMB.mid)
HIMB.df$log.deep<-log(HIMB.df$HIMB.deep)

HIMB.df$log.delta.mid.deep<-HIMB.df$log.mid-HIMB.df$log.deep # difference in 2m - 8m light
HIMB.df$log.delta.mid.shal<-HIMB.df$log.mid-HIMB.df$log.shal # difference in 2m - 1m light
HIMB.df$dm.depth<-HIMB.depths[3]-HIMB.depths[2] # difference in depth 8m - 2m
HIMB.df$sm.depth<-HIMB.depths[1]-HIMB.depths[2] # difference in depth 1m - 2m
HIMB.df$mm.depth<-HIMB.depths[2]-HIMB.depths[2] # difference in depth 2m - 2m

new.df<-HIMB.df[8:11]; colnames(new.df)<-c("log.PAR.delta", "log.PAR.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
PAR.HIMB<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

HIMB.mod<-lm(log.PAR.delta~Depth.delta+0, data=PAR.HIMB, na.action=na.exclude) # HIMB dataframe, depth continuous
HIMB.coef<-coef(HIMB.mod)[1] # use KD here to calculate light at any depth

# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
HIMB.df$coef.shal<-HIMB.df$HIMB.mid*exp(-HIMB.coef*HIMB.df$sm.depth)
HIMB.df$coef.mid<-HIMB.df$HIMB.mid*exp(-HIMB.coef*HIMB.df$mm.depth)
HIMB.df$coef.deep<-HIMB.df$HIMB.mid*exp(-HIMB.coef*HIMB.df$dm.depth)

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=HIMB.df$HIMB.shallow, x=HIMB.df$timestamp, type="l", col="dodgerblue", 
     main="HIMB logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="PAR", xlab="", ylim=c(0,1500))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$HIMB.mid, x=HIMB.df$timestamp, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$HIMB.deep, x=HIMB.df$timestamp, type="l", col="paleturquoise3"))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=HIMB.df$coef.shal, x=HIMB.df$timestamp, type="l", col="dodgerblue", ylab="PAR", 
     xlab="Date", ylim=c(0,1500))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.mid, x=HIMB.df$timestamp, type="l", col="gray"))
par(new=T)
with(HIMB.df, lines(y=HIMB.df$coef.deep, x=HIMB.df$timestamp, type="l", col="paleturquoise3"))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
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
PAR.Rf44$Depth<-as.numeric(PAR.Rf44$Depth) # depth as numeric
PAR.Rf44$log.PAR<-log(PAR.Rf44$PAR)

####
# make new dataframes for Sites
Rf44.df<-df[1:4] 
Rf44.depths<-c(1*0.3048, 6*0.3048, 25*0.3048)

####
# making some log data
Rf44.df$log.shal<-log(Rf44.df$Rf44.shallow)
Rf44.df$log.mid<-log(Rf44.df$Rf44.mid)
Rf44.df$log.deep<-log(Rf44.df$Rf44.deep)

Rf44.df$log.delta.mid.deep<-Rf44.df$log.mid-Rf44.df$log.deep # difference in 2m - 8m light
Rf44.df$log.delta.mid.shal<-Rf44.df$log.mid-Rf44.df$log.shal # difference in 2m - 1m light
Rf44.df$dm.depth<-Rf44.depths[3]-Rf44.depths[2] # difference in depth 8m - 2m
Rf44.df$sm.depth<-Rf44.depths[1]-Rf44.depths[2] # difference in depth 1m - 2m
Rf44.df$mm.depth<-Rf44.depths[2]-Rf44.depths[2] # difference in depth 2m - 2m

new.df<-Rf44.df[8:11]; colnames(new.df)<-c("log.PAR.delta", "log.PAR.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
PAR.Rf44<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

Rf44.mod<-lm(log.PAR.delta~Depth.delta+0, data=PAR.Rf44, na.action=na.exclude) # Rf44 dataframe, depth continuous
Rf44.coef<-coef(Rf44.mod)[1] # use KD here to calculate light at any depth

# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
Rf44.df$coef.shal<-Rf44.df$Rf44.mid*exp(-Rf44.coef*Rf44.df$sm.depth)
Rf44.df$coef.mid<-Rf44.df$Rf44.mid*exp(-Rf44.coef*Rf44.df$mm.depth)
Rf44.df$coef.deep<-Rf44.df$Rf44.mid*exp(-Rf44.coef*Rf44.df$dm.depth)

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf44.df$Rf44.shallow, x=Rf44.df$timestamp, type="l", col="dodgerblue", 
     main="Rf44 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="PAR", xlab="", ylim=c(0,1500))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$Rf44.mid, x=Rf44.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$Rf44.deep, x=Rf44.df$timestamp, type="l", col="paleturquoise3"))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=Rf44.df$coef.shal, x=Rf44.df$timestamp, type="l", col="dodgerblue", ylab="PAR", 
     xlab="Date", ylim=c(0,1500))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.mid, x=Rf44.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf44.df, lines(y=Rf44.df$coef.deep, x=Rf44.df$timestamp, type="l", col="paleturquoise3"))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_data_2xcoef_Rf44.pdf", height=4, width=6)
dev.off()


###################################
########### Reef 10 ############### 
PAR.Rf10<-PAR.df[(PAR.df$Site=="Rf10"),] # just Rf10
PAR.Rf10$Depth<-as.numeric(PAR.Rf10$Depth) # depth as factor

####
####
# make new dataframes for Sites
Rf10.df<-df[, c(1,5:7)]
Rf10.depths<-c(2*0.3048, 6*0.3048, 26*0.3048)

####
# making some log data
Rf10.df$log.shal<-log(Rf10.df$Rf10.shallow)
Rf10.df$log.mid<-log(Rf10.df$Rf10.mid)
Rf10.df$log.deep<-log(Rf10.df$Rf10.deep)

Rf10.df$log.delta.mid.deep<-Rf10.df$log.mid-Rf10.df$log.deep # difference in 2m - 8m light
Rf10.df$log.delta.mid.shal<-Rf10.df$log.mid-Rf10.df$log.shal # difference in 2m - 1m light
Rf10.df$dm.depth<-Rf10.depths[3]-Rf10.depths[2] # difference in depth 8m - 2m
Rf10.df$sm.depth<-Rf10.depths[1]-Rf10.depths[2] # difference in depth 1m - 2m
Rf10.df$mm.depth<-Rf10.depths[2]-Rf10.depths[2] # difference in depth 2m - 2m

new.df<-Rf10.df[8:11]; colnames(new.df)<-c("log.PAR.delta", "log.PAR.delta", "Depth.delta", "Depth.delta")
new.df1<-rbind(new.df[1], new.df[2]); new.df2<-rbind(new.df[3], new.df[4])
PAR.Rf10<-cbind(new.df1, new.df2) # for both columns has delta PAR and depth

Rf10.mod<-lm(log.PAR.delta~Depth.delta+0, data=PAR.Rf10, na.action=na.exclude) # Rf10 dataframe, depth continuous
Rf10.coef<-coef(Rf10.mod)[1] # use KD here to calculate light at any depth

# making some data
# notice that you need kd to be (+) here--or do absolute value of depth
Rf10.df$coef.shal<-Rf10.df$Rf10.mid*exp(-Rf10.coef*Rf10.df$sm.depth)
Rf10.df$coef.mid<-Rf10.df$Rf10.mid*exp(-Rf10.coef*Rf10.df$mm.depth)
Rf10.df$coef.deep<-Rf10.df$Rf10.mid*exp(-Rf10.coef*Rf10.df$dm.depth)

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf10.df$Rf10.shallow, x=Rf10.df$timestamp, type="l", col="dodgerblue", 
     main="Rf10 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="PAR", xlab="", ylim=c(0,1500))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$Rf10.mid, x=Rf10.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$Rf10.deep, x=Rf10.df$timestamp, type="l", col="paleturquoise3"))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=Rf10.df$coef.shal, x=Rf10.df$timestamp, type="l", col="dodgerblue", ylab="PAR", 
     xlab="Date", ylim=c(0,1500))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.mid, x=Rf10.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf10.df, lines(y=Rf10.df$coef.deep, x=Rf10.df$timestamp, type="l", col="paleturquoise3"))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)

dev.copy(pdf, "figures/environmental/PAR_data_2xcoef_Rf10.pdf", height=4, width=6)
dev.off()

########
########
# combine all coefficients
all.coefficients<-rbind(Rf42.coef, HIMB.coef, Rf44.coef, Rf10.coef)
write.csv(all.coefficients, "data/environmental/light coeff_cont.csv")

###########
########### End
########### 
