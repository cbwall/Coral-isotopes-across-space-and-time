# light attenuation with Depth as FACTOR to show light among depth zones
# this script will take the light at 2m and determine offsets at defined depth (<1m and 8m) based on loggers
# non-linear models are run and observed-expected are graphed
# a comparison of real and modeled data are also graphed to ensure agreement
# finally, a figure is made to show light environments at 3 depth environments for each site


####################################################
####################################################
### Light attenuation  October deployment ##########
####################################################
####################################################

# using October deployment at 3 depths

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
PAR.Rf42$Depth <- factor(PAR.Rf42$Depth, levels = c("2.4384", "0.9144", "8.2296"))
PAR.Rf42$log.PAR<-log(PAR.Rf42$PAR)

mod<-lm(log.PAR~Depth, data=PAR.Rf42, na.action=na.exclude) # Rf42 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf42.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])

shal<-Rf42.coeffs[2]; mid<-Rf42.coeffs[1]; deep<-Rf42.coeffs[3]

####
####
# make new dataframes for Sites
Rf42.df<-df[1:4] 

####
# making some log data
Rf42.df$log.shal<-log(Rf42.df$Rf42.shallow)
Rf42.df$log.mid<-log(Rf42.df$Rf42.mid)
Rf42.df$log.deep<-log(Rf42.df$Rf42.deep)

# making some log data using coefficients
Rf42.df$coef.shal<-Rf42.df$log.mid + shal
Rf42.df$coef.mid<-Rf42.df$log.mid
Rf42.df$coef.deep<-Rf42.df$log.mid + deep

##################
##### Figure #####
# how does data compare?

##### plot log data collected at each site
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(y=Rf42.df$log.shal, x=Rf42.df$timestamp, type="l", col="dodgerblue", 
     main="Reef 42 logger (top), 2m-coeff (bottom)", cex.main=0.7, 
     ylab="log(PAR)", xlab="", ylim=c(0,8))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$log.mid, x=Rf42.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$log.deep, x=Rf42.df$timestamp, type="l", col="paleturquoise3"))

##### plot of modeled 1m and 8m using <1m model coeffs. 
plot(y=Rf42.df$coef.shal, x=Rf42.df$timestamp, type="l", col="dodgerblue", ylab="log(PAR)", 
     xlab="Date", ylim=c(0,8))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.mid, x=Rf42.df$timestamp, type="l", col="gray"))
par(new=T)
with(Rf42.df, lines(y=Rf42.df$coef.deep, x=Rf42.df$timestamp, type="l", col="paleturquoise3"))
legend("top", lty=1, col=c("dodgerblue", "gray", "paleturquoise3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(0, -0.5), seg.len=1, cex=1.1, xpd=TRUE, horiz=TRUE)


################################ 
########### HIMB ############### 
PAR.HIMB<-PAR.df[(PAR.df$Site=="HIMB"),] # just HIMB
PAR.HIMB$Depth<-as.factor(PAR.HIMB$Depth) # depth as factor
PAR.HIMB$Depth <- factor(PAR.HIMB$Depth, levels = c("1.8288", "0.3048", "7.62"))

mod<-lm(log(PAR)~Depth, data=PAR.HIMB, na.action=na.exclude) # HIMB dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
HIMB.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])
shal<-HIMB.coeffs[2]; mid<-HIMB.coeffs[1]; deep<-HIMB.coeffs[3]

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
HIMB.df$coef.shal<-HIMB.df$log.mid + shal
HIMB.df$coef.mid<-HIMB.df$log.mid 
HIMB.df$coef.deep<-HIMB.df$log.mid + deep


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



####################################################
####################################################
### Light attenuation  November deployment #########
####################################################
####################################################

# using November deployment at 3 depths

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

Rf10.shall<-Nov.PAR[, c(1,5)]; Rf10.shall$Site<-as.factor("Rf10"); Rf10.shall$Depth<-2*0.3048 # logger at 2 ft
Rf10.mid<-Nov.PAR[, c(1,6)]; Rf10.mid$Site<-as.factor("Rf10"); Rf10.mid$Depth<-6*0.3048 # logger at 6 ft
Rf10.deep<-Nov.PAR[, c(1,7)]; Rf10.deep$Site<-as.factor("Rf10"); Rf10.deep$Depth<-26*0.3048 # logger at 26 ft

# bind together
PAR.df<-rbind(Rf44.shall, Rf44.mid, Rf44.deep, Rf10.shall, Rf10.mid, Rf10.deep)

# Break up into each reef
#####
PAR.Rf44<-PAR.df[(PAR.df$Site=="Rf44"),] # just Rf44
PAR.Rf44$Depth<-as.factor(PAR.Rf44$Depth) # depth as factor
PAR.Rf44$Depth <- factor(PAR.Rf44$Depth, levels = c("1.8288", "0.3048", "7.62"))
PAR.Rf44$log.PAR<-log(PAR.Rf44$PAR)

mod<-lm(log.PAR~Depth, data=PAR.Rf44, na.action=na.exclude) # R44 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf44.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])
shal<-Rf44.coeffs[2]; mid<-Rf44.coeffs[1]; deep<-Rf44.coeffs[3]

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



###################################
########### Reef 10 ############### 
PAR.Rf10<-PAR.df[(PAR.df$Site=="Rf10"),] # just Rf10
PAR.Rf10$Depth<-as.factor(PAR.Rf10$Depth) # depth as factor
PAR.Rf10$Depth <- factor(PAR.Rf10$Depth, levels = c("1.8288", "0.6096", "7.9248"))

mod<-lm(log(PAR)~Depth, data=PAR.Rf10, na.action=na.exclude) # Rf10 dataframe with Depth as a factor
summary(mod)$coefficients # coefficients
Rf10.coeffs<-c(coef(mod)["(Intercept)"], coef(mod)[2], coef(mod)[3])
shal<-Rf10.coeffs[2]; mid<-Rf10.coeffs[1]; deep<-Rf10.coeffs[3]

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


########
# combine all coefficients
all.coefficients<-rbind(Rf42.coeffs, HIMB.coeffs, Rf44.coeffs, Rf10.coeffs)
write.csv(all.coefficients, "data/environmental/light coeffs_factor.csv")

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

#dev.copy(pdf, "figures/environmental/PAR_coef_ob.ex.factor.pdf", height=6, width=6)
#dev.off()

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
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)


# Plot it!!
# Reef 42
plot(y=All.PAR.df$Rf42.shall, x=All.PAR.df$timestamp, type="l", col="dodgerblue", 
     cex.main=0.7, ylab=(expression(paste("PAR"~(mu*mol~photons~m^-2~s^-1), sep="")))
     , xlab="", main="Reef 42", cex.main=1, ylim=c(0, 2400))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf42.mid, x=All.PAR.df$timestamp, type="l", col="gray"))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf42.deep, x=All.PAR.df$timestamp, type="l", col="skyblue"))
legend("topright", lty=1, col=c("dodgerblue", "gray", "skyblue"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

# Plot it!!
# Reef 10
plot(y=All.PAR.df$Rf10.shall, x=All.PAR.df$timestamp, type="l", col="orangered", 
     cex.main=0.7, ylab=(expression(paste("PAR"~(mu*mol~photons~m^-2~s^-1), sep="")))
     , xlab="Dates", main="Reef 10", cex.main=1, ylim=c(0, 2400))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf10.mid, x=All.PAR.df$timestamp, type="l", col="gray"))
par(new=T)
with(All.PAR.df, lines(y=All.PAR.df$Rf10.deep, x=All.PAR.df$timestamp, type="l", col="lightsalmon3"))
legend("topright", lty=1, col=c("orangered", "gray", "lightsalmon3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

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
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

dev.copy(pdf, "figures/environmental/PAR.all depths.pdf", height=7, width=8)
dev.off()

#######
####### caluclate DLI and compare
#######
df<-All.PAR.df
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp, format="%Y-%m-%d") # change to DATE ONLY format to calulate range, means
df[is.na(df)] <- 0


df.split <- split(df, f=df$timestamp
                  < as.Date("2016-06-10", format="%Y-%m-%d")) # split df by date

df.dli<-aggregate(data.frame(Rf42.shall.DLI=df.split[[1]]$Rf42.shall*0.0864,
                             Rf42.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                             Rf42.deep.DLI=df.split[[1]]$Rf42.deep*0.0864,
                             Rf44.shall.DLI=df.split[[1]]$Rf44.shall*0.0864,
                             Rf44.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                             Rf44.deep.DLI=df.split[[1]]$Rf44.deep*0.0864,
                             HIMB.shall.DLI=df.split[[1]]$HIMB.shall*0.0864,
                             HIMB.mid.DLI=df.split[[1]]$HIMB.mid*0.0864,
                             HIMB.deep.DLI=df.split[[1]]$HIMB.deep*0.0864,
                             Rf10.shall.DLI=df.split[[1]]$Rf10.shall*0.0864,
                             Rf10.mid.DLI=df.split[[1]]$Rf10.mid*0.0864,
                             Rf10.deep.DLI=df.split[[1]]$Rf10.deep*0.0864),
                  by=list(Date=df.split[[1]]$timestamp), FUN=mean)


# make a date sequence for entire study
all.date.time<-as.data.frame(seq(
  from=as.POSIXct("2016-06-10", tz="HST"),
  to=as.POSIXct("2017-01-12", tz="HST"),
  by="1 d"))  
colnames(all.date.time)[1]<-"Date"
all.date.time$Date<-strptime(all.date.time$Date, format="%Y-%m-%d")
all.date.time$Date<-as.Date(all.date.time$Date, format="%Y-%m-%d")

df.dli<-merge(all.date.time, df.dli, by="Date", all.x=T)
df.dli[df.dli<=0] <- NA

# Plot it!!
# Reef 44
par(mfrow=c(2,2), mar=c(4,5,2,2))

plot(y=df.dli$Rf44.shall, x=df.dli$Date, type="l", col="palegreen2", 
     cex.main=0.7, ylab=(expression(paste("DLI"~(mol~photons~m^-2~d^-1), sep="")))
     , xlab="", main="Reef 44", cex.main=1, ylim=c(0, 50))
par(new=T)
with(df.dli, lines(y=df.dli$Rf44.mid, x=df.dli$Date, type="l", col="gray"))
par(new=T)
with(df.dli, lines(y=df.dli$Rf44.deep, x=df.dli$Date, type="l", col="palegreen4"))
legend("topright", lty=1, col=c("palegreen2", "gray", "palegreen4"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)


# Plot it!!
# Reef 42
plot(y=df.dli$Rf42.shall, x=df.dli$Date, type="l", col="dodgerblue", 
     cex.main=0.7, ylab=(expression(paste("DLI"~(mol~photons~m^-2~d^-1), sep="")))
     , xlab="", main="Reef 42", cex.main=1, ylim=c(0, 50))
par(new=T)
with(df.dli, lines(y=df.dli$Rf42.mid, x=df.dli$Date, type="l", col="gray"))
par(new=T)
with(df.dli, lines(y=df.dli$Rf42.deep, x=df.dli$Date, type="l", col="skyblue"))
legend("topright", lty=1, col=c("dodgerblue", "gray", "skyblue"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

# Plot it!!
# Reef 10
plot(y=df.dli$Rf10.shall, x=df.dli$Date, type="l", col="orangered", 
     cex.main=0.7, ylab=(expression(paste("DLI"~(mol~photons~m^-2~d^-1), sep="")))
     , xlab="Dates", main="Reef 10", cex.main=1, ylim=c(0, 50))
par(new=T)
with(df.dli, lines(y=df.dli$Rf10.mid, x=df.dli$Date, type="l", col="gray"))
par(new=T)
with(df.dli, lines(y=df.dli$Rf10.deep, x=df.dli$Date, type="l", col="lightsalmon3"))
legend("topright", lty=1, col=c("orangered", "gray", "lightsalmon3"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

# Plot it!
# HIMB
plot(y=df.dli$HIMB.shall, x=df.dli$Date, type="l", col="mediumorchid2", 
     cex.main=0.7, ylab=(expression(paste("DLI"~(mol~photons~m^-2~d^-1), sep="")))
     , xlab="Dates", main="HIMB", cex.main=1, ylim=c(0, 50))
par(new=T)
with(df.dli, lines(y=df.dli$HIMB.mid, x=df.dli$Date, type="l", col="gray"))
par(new=T)
with(df.dli, lines(y=df.dli$HIMB.deep, x=df.dli$Date, type="l", col="plum4"))
legend("topright", lty=1, col=c("mediumorchid2", "gray", "rosybrown2"), legend=c("<1m", "2m", "8m"), 
       lwd=2, bty="n", inset=c(-0.15, -0.05), seg.len=0.5, cex=0.9, x.intersp=0.2, y.intersp=0.4)

dev.copy(pdf, "figures/environmental/DLIcalc.all depths.pdf", height=7, width=8)
dev.off()

#######
####### End
#######

