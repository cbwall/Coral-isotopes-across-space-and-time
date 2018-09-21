main<-setwd(getwd())
setwd("~/Desktop/Research and Teaching/UH MANOA/Research/Pan KBay isotopes/R")
rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(plotrix)
library(lubridate)
library(effects)
library(RCurl)
library(chron)
library(lubridate)
library(lme4)
library(emmeans)
library(multcomp)
library(devtools)
library(logihist)
library(popbio)
library(car)
library(lmerTest)
library(cowplot)

#
####
########
###############
#########################

#
####
########
###############
#########################
# Sea level correction
#########################

#### attach data files
data<-read.csv("data/mastersheet_PanKBAY.csv") # master file
qPCR.Innis<-read.csv("data/PanKBay_summer_qPCR.csv") # qPCR from summer (Innis et al. 2018)

JuneTide=read.csv("data/environmental/sea level/Station_1612480_tide_ht_20160601-20160630.csv")
JulyTide=read.csv("data/environmental/sea level/Station_1612480_tide_ht_20160701-20160731.csv")
AugustTide=read.csv("data/environmental/sea level/Station_1612480_tide_ht_20160801-20160831.csv")
DecemberTide=read.csv("data/environmental/sea level/Station_1612480_tide_ht_20161201-20161222.csv")

Tide<-rbind(JuneTide, JulyTide, AugustTide, DecemberTide) # all MSL in meters

Tide$Time <- as.POSIXct(Tide$TimeUTC, format="%Y-%m-%d %H:%M:%S", tz="UTC")
attributes(Tide$Time)$tzone <- "Pacific/Honolulu"
# use attr(as.POSIXlt(Tide$Time),"tzone") to confirm in PST

Tide<-Tide[, c(-5:-8)] #remove unnecessary columns

data$date.time<- as.POSIXct(paste(as.character(data$Date), as.character(data$Time.of.collection)),
                 format="%m/%d/%y %H:%M", tz="Pacific/Honolulu") # make sure time in HST for master

data$Time.r<-round_date(data$date.time,unit="6 minutes")
Tide$Time.r <- Tide$Time
data<-merge(data, Tide, by="Time.r", all.x=T)
data$newDepth <- data$Depth..m-data$TideHT # new depth in METERS


###########################################
########################## # LIGHT AT DEPTH
#############
###### 
# using new depth and the site-specific kd (light attenuation) determine approximate "light at depth" for each colony sample. 

######

### attach necessary files
logger.depths<-read.csv("data/environmental/temp and light/light.logger.depths.csv") # depths for loggers
kd.all<-read.csv("data/environmental/kd.all.csv") # kds for each site

# Seasonal DLI used for "period of collection" light levels
month.DLI<-read.csv("data/environmental/temp and light/Jun_DecPAR/All.DLI_long.csv")

# corals collected in June-July-August use summer time DLI for these months as indicator of average DLI
summer.DLI<-month.DLI[(month.DLI$Month=="June" | month.DLI$Month=="July" | month.DLI$Month=="August"),]
winter.DLI<-month.DLI[(month.DLI$Month=="November" | month.DLI$Month=="December" |month.DLI$Month=="January"),]

# summer mean and SE dataframe
sum.mean<-aggregate(DLI~Site, summer.DLI, mean)
sum.SE<-aggregate(DLI~Site, summer.DLI, std.error)
sum.light.df<-cbind(sum.mean, sum.SE[2])
colnames(sum.light.df)=c("Site", "mean.DLI", "SE")
sum.light.df$Season<-as.factor("summer")

# winter mean and SE dataframe
wint.mean<-aggregate(DLI~Site, winter.DLI, mean)
wint.SE<-aggregate(DLI~Site, winter.DLI, std.error)
wint.light.df<-cbind(wint.mean, wint.SE[2])
colnames(wint.light.df)=c("Site", "mean.DLI", "SE")
wint.light.df$Season<-as.factor("winter")

season.DLI<-rbind(sum.light.df[,c(4,1,2:3)], wint.light.df[,c(4,1,2:3)]) # compiled means for DLI at ~2m

write.csv(season.DLI, "data/environmental/season.DLI.csv")
#######################################
### make new dataframe for calculations
df.light<-data %>% 
  select(Season, Location, newDepth)

# make a column of "depth differences" relative to where ~2m logger was deployed
df.light$depth.diff<-ifelse(df.light$Location=="F1-R46", df.light$newDepth-1.8288,
                          ifelse(df.light$Location=="F8-R10", df.light$newDepth-1.8288, 
                                 ifelse(df.light$Location=="HIMB", df.light$newDepth-1.8288, 
                                        df.light$newDepth-2.4384))) # last statement for remaining site (Rf42)

# make a column for sample-specific light at depth (estimate) based on kd
# follow: #with 2m as baseline#  E(depth) = E(2m)*exp(-k_d*(depth - 2m))
# so that:     light at depth = DLI at mid.depth * exp (-kd *(delta shallow-deep in m))

df.light$Light<-ifelse(df.light$Location=="HIMB" & df.light$Season=="summer", 
                          season.DLI$mean.DLI[1]*exp(-kd.all$kd[1]*df.light$depth.diff), # summer HIMB
                   ifelse(df.light$Location=="F8-R10" & df.light$Season=="summer", 
                          season.DLI$mean.DLI[2]*exp(-kd.all$kd[2]*df.light$depth.diff), # summer R10
                   ifelse(df.light$Location=="R42" & df.light$Season=="summer", 
                          season.DLI$mean.DLI[3]*exp(-kd.all$kd[3]*df.light$depth.diff), # summer R42
                   ifelse(df.light$Location=="F1-R46" & df.light$Season=="summer", 
                                 season.DLI$mean.DLI[4]*exp(-kd.all$kd[4]*df.light$depth.diff), # summer R46
                          
                   ifelse(df.light$Location=="HIMB" & df.light$Season=="winter", 
                          season.DLI$mean.DLI[5]*exp(-kd.all$kd[1]*df.light$depth.diff), # winter HIMB
                   ifelse(df.light$Location=="F8-R10" & df.light$Season=="winter", 
                          season.DLI$mean.DLI[6]*exp(-kd.all$kd[2]*df.light$depth.diff), # winter R10
                   ifelse(df.light$Location=="R42" & df.light$Season=="winter", 
                          season.DLI$mean.DLI[7]*exp(-kd.all$kd[3]*df.light$depth.diff), # winter R42
                          season.DLI$mean.DLI[8]*exp(-kd.all$kd[4]*df.light$depth.diff)) # winter R46
                            ))))))

###### plot of light x depth by season
df.light$Location <- factor(df.light$Location, levels = c("F1-R46", "R42", "F8-R10", "HIMB"))

plot.by.sites=ggplot(df.light, aes(Light)) + geom_density(aes(fill=Location), alpha=0.3, position = 'stack') + scale_x_continuous(limits=c(0, 40)) + ggtitle("Light at Depth by Seasons") 

plot.by.site=ggplot(df.light, aes(Light)) + geom_density(aes(fill=Season), alpha=0.3, position = 'stack') +
  scale_x_continuous(limits=c(0, 40)) + ggtitle("Light at Depth by Seasons") + facet_wrap(~Location, scales="free") + scale_fill_manual(values=c("darkorange1", "dodgerblue1"))

######
# can loop as a list
p=ggplot(df.light, aes(Light, fill=Season)) + geom_density(alpha=0.3, position = 'stack') + scale_x_continuous(limits=c(0, 40)) + ggtitle("Light at Depth by Seasons") 

plots=dlply(df.light, .(Location), function(x) p %+% x + facet_wrap(~Location))


###### plot of light x depth by season with exponential curve fitting
Sum<-df.light[(df.light$Season=="summer"),]
Win<-df.light[(df.light$Season=="winter"),]

plot(Light~newDepth, Sum, col="coral", pch=16, xlab="Depth (m)", ylab="DLI at coral", 
     ylim=c(0, 30), xlim=c(0, 10))
summod<-glm(Light~newDepth, family=poisson, Sum)
curve(exp(coef(summod)["(Intercept)"]+coef(summod)["newDepth"]*x), add=TRUE, col="coral", lwd=2)
par(new=T)
plot(Light~newDepth, Win, col="lightblue2", pch=16, xaxt="n", yaxt="n", 
     xlab="", ylab="", ylim=c(0, 30), xlim=c(0, 10))
wintmod<-glm(Light~newDepth, family=poisson, Win)
curve(exp(coef(wintmod)["(Intercept)"]+coef(wintmod)["newDepth"]*x), add=TRUE, col="lightblue2", lwd=2)
legend("topright", legend=c("summer", "winter"), col=c("coral", "lightblue2"), lty=1, lwd=1, pch=16, bty="n")



#
####
########
###############
#########################
################################
### Biological responses
################################

#data : this is the master file

# add in light at depth column from df.light dataframe
data$Light<-df.light$Light

##### produce a categorical depth bin ####
depth<-data$newDepth
data$depth.bin<-factor(ifelse(depth<2, "<2m", ifelse(depth >2 & depth <4, "2-4m", ifelse(depth >4 & depth <6, "4-6m", ">6m"))), levels=c("<2m", "2-4m", "4-6m", ">6m"))

aggregate(Sample.ID~depth.bin+Season+Location, data, length)
data$depth.bin.small<-factor(ifelse(depth<4, "<4m", ">4m"), levels= c("<4m", ">4m"))

################################################
# calculate, normalized dependent variables
################################################
str(data)
data$cells.ml<-as.numeric(data$cells.ml)

# helpful shorthand
SA<-data$surface.area # surface area in cm2
blastate<-data$total.blastate.ml # tissue slurry blastate in ml

# AFDW.mg. == convert AFDW g to mg, mutiply by blastate volume, divide by cm2
data$biomass<- (data$mg.biomass.ml*blastate)/SA

# Symbiodinium.cells. == cell.ml * blastate / SA
data$zoox<- (data$cells.ml*blastate)/SA

# total chlorophyll == ug.chl.a.ml * blastate + ug.chl.c2.ml * blastate / SA
data$chltot<-(data$ug.chl.a.ml)+(data$ug.chl.c2.ml)*blastate/SA

# pg.chlorophyll.a..cell + pg.chlorophyll.c2..cell == ug.chltot.ml * 10^6 / cells.ml
data$chlcell<- (data$ug.chl.a.ml*10^6+data$ug.chl.c2.ml*10^6)/data$cells.ml


######################################
############################
####################
# qPCR
#########

# qPCR
# Use steponeR to import data and calculate proporation of C and D symbionts
source_url("https://raw.githubusercontent.com/jrcunning/steponeR/master/steponeR.R")
Mcap.plates <- list.files(path="data/qPCR", pattern = "txt$", full.names = T); Mcap.plates
Mcap <- steponeR(files=Mcap.plates, delim="\t",
                 target.ratios=c("C.D"),
                 fluor.norm=list(C=2.26827, D=0),
                 copy.number=list(C=33, D=3),
                 ploidy=list(C=1, D=1), 
                 extract=list(C=0.813, D=0.813))

Mcap <- Mcap$result
head(Mcap)

# remove +/-control
Mcap <- Mcap[grep("+C52", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("H2O", Mcap$Sample.Name, fixed=T, invert = T), ]

# to remove any early-amplification CT noise
Mcap$C.CT.mean[which(Mcap$C.CT.mean < 15)] <- 0

#Remove failed samples, i.e., those where either C or D were NOT found in both reps
Mcap$fail <- ifelse(Mcap$C.reps < 2 & Mcap$D.reps < 2, TRUE, FALSE)
fails <- Mcap[Mcap$fail==TRUE, ]
Mcap <- Mcap[which(Mcap$fail==FALSE),]

# replace CT means with 'NA' as zero
Mcap$C.CT.mean[is.na(Mcap$C.CT.mean)] <-0
Mcap$D.CT.mean[is.na(Mcap$D.CT.mean)] <-0

Mcap$C.D[is.na(Mcap$C.D)] <- 1 # sets all infinity (= 100% C) to 1.0

# caluclate proportion C and proprtion D where C and D are both present
Mcap$propC<- Mcap$C.D / (Mcap$C.D + 1)
Mcap$propD<- 1 / (Mcap$C.D + 1)

# where C and D are not cooccuring...
# if C.D = 1 = 100% C, make 'PropC' = 1 and 'PropD' = 0
# if C.D = 0 = 100% D, make 'PropD' = 1 and 'PropC' = 0
Mcap$propC[which(Mcap$C.D==1)] <- 1
Mcap$propD[which(Mcap$propC==1)] <- 0
Mcap$propD[which(Mcap$C.D==0)] <- 1

# calculate FOUR COMMUNITY categories: C, C>D, D>C, D
Mcap$Mix <- factor(ifelse(Mcap$propC > Mcap$propD, ifelse(Mcap$propD!= 0, "CD", "C"), ifelse(Mcap$propD > Mcap$propC, ifelse(Mcap$propC!=0, "DC", "D"), NA)), levels=c("C", "CD", "DC", "D"))

# Identify SINGLE dominant symbiont clade: C or D
Mcap$Dom <- factor(substr(as.character(Mcap$Mix), 1, 1))

# Set zeros to NA to facilitate log transformation
Mcap$propC[which(Mcap$propC==0)] <- NA
Mcap$propD[which(Mcap$propD==0)] <- NA

######## look for duplicates in dataset by year and type of event (bleach/recover)
Mcap[duplicated(Mcap$Sample.Name), ] ## duplicates

# remove duplicated
Mcap<-Mcap[!(Mcap$Sample.Name=="HIMB_15" & Mcap$File.Name=="Wall_PanKbay_plate1.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="R10_05" & Mcap$File.Name=="Wall_PanKbay_plate1.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="R42_06" & Mcap$File.Name=="Wall_PanKbay_plate1.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="R46_01" & Mcap$File.Name=="Wall_PanKbay_plate1.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="R46_02" & Mcap$File.Name=="Wall_PanKbay_plate2.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="R46_03" & Mcap$File.Name=="Wall_PanKbay_plate1.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="HIMB_13" & Mcap$File.Name=="Wall_PanKbay_plate2.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="HIMB_14" & Mcap$File.Name=="Wall_PanKbay_plate2.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="R10_15" & Mcap$File.Name=="Wall_PanKbay_plate3.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="R10_15" & Mcap$File.Name=="Wall_PanKbay_plate1.txt"),] 
Mcap<-Mcap[!(Mcap$Sample.Name=="R42_11" & Mcap$File.Name=="Wall_PanKbay_plate2.txt"),] 

# parse Sample ID and Site from "Site.Name"
Mcap<-cbind(Mcap, colsplit(Mcap$Sample.Name, pattern= "_", c("Location", "Sample.ID")))
Mcap$Season<-as.factor("winter")
Mcap$Location<-as.factor(Mcap$Location)

Mcap$Location<-revalue(Mcap$Location, c("R10"="F8-R10", "R46"="F1-R46")) # rename factor levels

# make new factors for bay region and reef type
Mcap$Bay.region <- ifelse(Mcap$Location=="R42" | Mcap$Location=="F1-R46", "northern", "southern")
Mcap$Reef.type <- ifelse(Mcap$Location=="R42" | Mcap$Location=="HIMB", "patch", "fringe")

### reorder columns and finish
Mcap<-Mcap[, c(17,15,19,18,16, 1:14)] # reordered to match masterdata, and finish

### structure winter and summer qPCR dataframes to have same columns and combine dataframe 
qPCR.winter<-Mcap[ , (names(Mcap) %in% 
            c("Season", "Location", "Reef.type", "Bay.region", "Sample.ID", "propC", "propD", "Mix", "Dom"))]
qPCR.summer<-qPCR.Innis[ , (names(qPCR.Innis) %in% 
            c("Season", "Location", "Reef.type", "Bay.region", "Sample.ID", "propC", "propD", "Mix", "Dom"))] 

# merge qPCR files
qPCR.all<-rbind(qPCR.winter, qPCR.summer)

# add to master data
data.all<-merge(data, qPCR.all, by=c("Season", "Location", "Reef.type", "Bay.region", "Sample.ID"), all.x=T)

###### remove columns no longer needed, update "Depth" to be tide-corrected depth )= newDepth
data.trim<-data.all[ , !(names(data.all) %in% c("total.blastate.ml", "Date", "Time.of.collection", "Depth..m", "Time.r", "surface.area.cm2", "cells.ml", "ug.chl.a.ml", "ug.chl.c2.ml", "mg.biomass.ml", "host..mass.mg", "host..ugN", "host..ugC", "symb..mass.mg", "symb..ugN", "symb..ugC", "stationId", "datum", "TimeUTC", "TideHT", "Time"))]

data.trim$symb..C.N[data.trim$symb..C.N>=12.520270]=NA # set this outlier to NA

#####################################
##########################
################
######### MODELS

###### model dataframe for analysis
#####
model.data<-data.trim[,c(16, 1:4, 17:24, 6:15, 25:28)]

##########
##########

##transformations
model.data$zoox<-log(model.data$zoox)
model.data$chlcell<-log(model.data$chlcell)
model.data$host..C.N<-log(model.data$host..C.N)
model.data$symb..C.N<-log(model.data$symb..C.N)
model.data$d13C..host.sym<-sign(model.data$d13C..host.sym)*log(abs(model.data$d13C..host.sym)+1)

## tests for assumptions
for(i in c(10:22)){
  Y<-model.data[,i]
  full<-lmer(Y~Season*Light*Dom + (1|Location), data=model.data, na.action=na.exclude)
  R <- resid(full) #save glm residuals
  Y.shapiro <- shapiro.test(R) #runs a normality test on residuals
  print(Y.shapiro) # null = normally distrubuted (P<0.05 = non-normal
  
  op<-par(mfrow = c(1,2), mar=c(5,4,1,2), pty="sq")
  plot(full, add.smooth = FALSE, which=1)
  QQ <- qqnorm(R, main = colnames(model.data)[i]) 
  QQline <- qqline(R)
  hist(R, xlab="Residuals", main = colnames(model.data)[i])
}

########## ########## 
######### biomass ---- 
Y<-model.data$biomass
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
summary(full)
print(anova(full, type=2), digits=4)

library(sjPlot)
library(sjmisc)
ranef(full)
fixef(full)
sjp.lmer(full, y.offset = .4)
sjp.lmer(full, vars = "Seasonwinter", type = "ri.slope")

posthoc<-emmeans(full, ~Light:Season)
CLD(posthoc, Letters=letters)
plot(allEffects(full), ylab="biomass", par.strip.text=list(cex=0.7))



########## ########## 
######### chltotal-- 
Y<-model.data$chltot
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
summary(full)
print(anova(full, type=2), digits=4)

posthoc<-emmeans(full, ~Dom:Light:Season)
CLD(posthoc, Letters=letters)

plot(allEffects(full), ylab="total chlor", par.strip.text=list(cex=0.7))
ranef(full)
fixef(full)
sjp.lmer(full, y.offset = .4)


######### ######### 
######### chlcell -- 
Y<-model.data$chlcell
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+ (1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use additive model

summary(add)
print(anova(add, type=2), digits=5)

plot(allEffects(add), ylab="chla cell", par.strip.text=list(cex=0.7))
ranef(add)
rand(add)
fixef(add)
sjp.lmer(full, y.offset = .4)

########## ########## 
########## host..d13C -- 
Y<-model.data$host..d13C
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+ (1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use additive model

summary(add)
print(anova(add, type=2), digits=5)

plot(allEffects(add), ylab="d13Chost", par.strip.text=list(cex=0.7))
ranef(add)
rand(add)
fixef(add)
sjp.lmer(full, y.offset = .4)


########## ########## 
########## symb..d13C --
Y<-model.data$symb..d13C
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+ (1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use additive model

summary(add)
print(anova(add, type=2), digits=5)

plot(allEffects(add), ylab="d13Csymb", par.strip.text=list(cex=0.7))
ranef(add)
rand(add)
fixef(add)
sjp.lmer(full, y.offset = .4)


########## ########## 
######### d13C..host.sym --
Y<-model.data$d13C..host.sym
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+ Season:Light +Season:Dom +(1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use add model

summary(add)
print(anova(add, type=2), digits=4)

plot(allEffects(add), ylab="d13C-hs", par.strip.text=list(cex=0.7))
ranef(add)
rand(add)
fixef(add)
sjp.lmer(add, y.offset = .4)


########## ########## 
####### host..d15N --
Y<-model.data$host..d15N
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+Season:Light+ Season:Dom+ Light:Dom +(1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use add model

summary(add)
print(anova(add, type=2), digits=4)

plot(allEffects(full), ylab="d15Nhost", par.strip.text=list(cex=0.7))
ranef(add)
rand(add)
fixef(add)
sjp.lmer(add, y.offset = .4)


############# #############
######## symb..d15N --
Y<-model.data$symb..d15N
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+Season:Light+ Season:Dom+ Light:Dom + (1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use full model

summary(add)
print(anova(add, type=2), digits=3)

posthoc<-emmeans(full, ~Season*Dom)
CLD(posthoc, Letters=letters)

plot(allEffects(full), ylab="d15Nsymb", par.strip.text=list(cex=0.7))
ranef(add)
rand(add)
fixef(add)
sjp.lmer(add, y.offset = .4)


############# #############
####### d15N..host.sym  -- 
Y<-model.data$d15N..host.sym
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+ Season:Dom+  (1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use full model

summary(add)
print(anova(add, type=2), digits=3)

posthoc<-emmeans(full, ~Season*Dom)
CLD(posthoc, Letters=letters)

plot(allEffects(add), ylab="d15N-hs", par.strip.text=list(cex=0.7))
ranef(add)
rand(add)
fixef(add)
sjp.lmer(add, y.offset = .4)


############# #############
###### host..C.N --
Y<-model.data$host..C.N
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+ (1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use full model

summary(add)
print(anova(add, type=2), digits=4)

plot(allEffects(add), ylab="C:N host", par.strip.text=list(cex=0.7))
ranef(add)
rand(add)
fixef(add)
sjp.lmer(add, y.offset = .4)


############# #############
####### symb..C.N -- 
Y<-model.data$symb..C.N
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+ (1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use full model

summary(add)
print(anova(add, type=2), digits=5)

plot(allEffects(full), ylab="C:N symb", par.strip.text=list(cex=0.7))



############# #############
####### d13C..skel --
Y<-model.data$d13C..skel
full<-lmer(Y~Season*Light*Dom+ (1|Location), data=model.data, na.action=na.exclude)
add<-lmer(Y~Season+Light+Dom+ (1|Location), data=model.data, na.action=na.exclude)
anova(full, add) #use full model

summary(add)
print(anova(add, type=2), digits=4)

posthoc<-emmeans(add, ~Season)
CLD(posthoc, Letters=letters)
plot(allEffects(full), ylab="d13C-skel", par.strip.text=list(cex=0.7))



#########################
#########################
#########################
## Figures
#########################
#########################
#########################

#qPCR and symbionts

#########################
###### Plot Dominant Symbiont and Depth (both seasons)
Symb<-model.data
Symb$Dominant <- ifelse(Symb$Dom=="D", 1, 0)
results.all=glm(Dominant~newDepth, family = "binomial", data = Symb)
anova(results.all, test = "Chisq")
summary(results.all)
fitted.all <- predict(results.all, newdata = list(newDepth=seq(0,12,0.1)), type = "response")
plot(fitted.all, ylab="proportion D", ylim=c(0,1))

###### summer only symbionts
sum.dat<-Symb[(Symb$Season=="summer"),]
results.sum=glm(Dominant~newDepth, family = "binomial", data = sum.dat)
anova(results.sum, test = "Chisq")
summary(results.sum)
fitted.sum <- predict(results.sum, newdata = list(newDepth=seq(0,12,0.1)), type = "response")
plot(fitted.sum, ylab="proportion D", ylim=c(0,1))

###### winter only symbionts
wint.dat<-Symb[(Symb$Season=="winter"),]
results.win=glm(Dominant~newDepth, family = "binomial", data = wint.dat)
anova(results.win, test = "Chisq")
summary(results.win)
fitted.win <- predict(results.win, newdata = list(newDepth=seq(0,12,0.1)), type = "response")
plot(fitted.win, ylab="proportion D", ylim=c(0,1))

######
######
## Figure of Dominant symbiont clades across seasons
## **Note** where points equal 0.0 is 100% D, where they equal 0 is 100% C
par(mar=c(5,4,3,2))
plot(sum.dat$propD~sum.dat$newDepth, xlab="Depth (m)", ylab = "Proportion of Clade D Symbiont", pch=19, col="salmon", xlim=c(0,10), ylim=c(0.0, 1.0), cex=0.7)
par(new=T)
plot(wint.dat$propD~wint.dat$newDepth, pch=19, col="lightskyblue", xlim=c(0,10), ylim=c(0.0, 1.0), xaxt="n", xlab="", ylab="", cex=0.7)
lines(fitted.all ~ seq(0,12,0.1), col="gray30", lwd=1.5, lty=2)
lines(fitted.sum ~ seq(0,12,0.1), col="coral", lwd=1.5)
lines(fitted.win ~ seq(0,12,0.1), col="lightskyblue", lwd=1.5)
legend("topright", pch=c(19,19, NA), lty=2, col=c("coral", "lightskyblue", "gray30"), legend=c("Summer", "Winter", "Combined"), lwd=1.5, bty="n", x.intersp=0.2, pt.cex=0.7, y.intersp=0.7, cex=0.8, inset=c(-0.06, 0.1), seg.len=1.1)
dev.print(pdf, "figures/symbionts/Symbionts_by_Season.pdf", encod="MacRoman", height=4, width=5)
dev.off()

#######
#######
par(mfrow=c(1,2))
Dom1 <- subset(Symb, !is.na(newDepth) & !is.na(Dominant))
Dom.sum<-Dom1[(Dom1$Season=="summer"),]
Dom.win<-Dom1[(Dom1$Season=="winter"),]

logi.hist.plot(Dom.sum$newDepth, Dom.sum$Dominant, boxp = FALSE, type = "hist", col="coral", xlabel = "Depth (m)", ylabel = "", ylabel2 = "", main="summer")
mtext(side = 2, text = "Probability of Clade D", line = 3, cex = 1)

logi.hist.plot(Dom.win$newDepth, Dom.win$Dominant, boxp = FALSE, type = "hist", col="lightskyblue", xlabel = "Depth (m)", ylabel = "", ylabel2 = "", main="winter")
mtext(side = 4, text = "Frequency", line = 0.5, cex=1)
mtext(side = 4, text = "C                                             D", line = 0.5, cex = 0.8)
dev.print(pdf, "figures/symbionts/Symbionts_Season_logistic.pdf", encod="MacRoman", height=5, width=8)
dev.off()



#########################
#########################
# Physiology
df.fig<-data.trim
# figure layout
layout(matrix(c(1,1,2,3), 2,2, byrow=TRUE))
par(mar=c(5,4.5,2,2))

##################
################## 

## season relationship
data.summer<-df.fig[df.fig$Season=="summer", ]
data.winter<-df.fig[df.fig$Season=="winter", ]

## Site relationship
F8.R10.df<-df.fig[df.fig$Location=="F8-R10", ]
F1.R46.df<-df.fig[df.fig$Location=="F1-R46", ]
HIMB.df<-df.fig[df.fig$Location=="HIMB", ]
R42.df<-df.fig[df.fig$Location=="R42", ]

## Symbiont relationship
C.sum.df<-df.fig[(df.fig$Dom=="C" & df.fig$Season=="summer") ,]
C.win.df<-df.fig[(df.fig$Dom=="C" & df.fig$Season=="winter") ,]; C.win.df<-na.omit(C.win.df)
D.sum.df<-df.fig[(df.fig$Dom=="D" & df.fig$Season=="summer") ,]; D.sum.df<-na.omit(D.sum.df)
D.win.df<-df.fig[(df.fig$Dom=="D" & df.fig$Season=="winter") ,]; D.win.df<-na.omit(D.win.df)

##################
# Fig: chlorophyll (total) over season and depth
################## 

plot(chltot~newDepth, data=data.winter,col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab=expression(paste("chlorophyll", ~(mu*g~cm^-2), sep="")), 
     main="chl: Depth and Season",
     xlim=c(0,11), ylim=c(1,17))
abline((lm(chltot~newDepth, data=data.winter)), col='dodgerblue3', lwd=2)
points(chltot~newDepth, data=data.summer, col="tomato2", pch=16, cex=0.7)
abline((lm(chltot~newDepth, data=data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(data.winter$chltot), ylim=c(0,0.3), xlim=c(0, 18), col="dodgerblue3", main="chl: Seasons", 
     xlab=expression(paste("chlorophyll", ~(mu*g~cm^-2), sep="")))
lines(density(data.summer$chltot), col="tomato2")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.35, -0.1))


###### density plot: by Site
plot(density(F1.R46.df$chltot), ylim=c(0,0.3), xlim=c(0, 18), col="tomato2", main="chl: Sites", 
     xlab=expression(paste("chlorophyll", ~(mu*g~cm^-2), sep="")), ylab="Density")
lines(density(R42.df$chltot), col="skyblue3")
lines(density(F8.R10.df$chltot), col="springgreen4")
lines(density(HIMB.df$chltot), col="purple")
legend("topright", c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))
dev.print(pdf, "figures/physiology/PanKB.chl.pdf", width=9, height=7)



##################
# Fig: chlorophyll (total) over season and depth
################## 

plot(chltot~Light, data=C.sum.df,col="tomato2", pch=16, cex=0.7, 
     xlab="Light (DLI)", ylab=expression(paste("chlorophyll", ~(mu*g~cm^-2), sep="")), 
     main="chl: Light and Season",
     xlim=c(0,20), ylim=c(1,12))
abline((lm(chltot~Light, data=C.sum.df)), col='tomato2', lwd=2)
points(chltot~Light, data=C.win.df, col="dodgerblue3", pch=16, cex=0.7)
abline((lm(chltot~Light, data=C.win.df)), col='dodgerblue3', lwd=2)
points(chltot~Light, data=D.sum.df, col="mediumseagreen", pch=16, cex=0.7)
abline((lm(chltot~Light, data=D.sum.df)), col='mediumseagreen', lwd=2)
points(chltot~Light, data=D.win.df, col="orchid", pch=16, cex=0.7)
abline((lm(chltot~Light, data=D.win.df)), col='orange', lwd=2)
legend("topright", c("C-sum", "C-win", "D-sum", "D-win"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3","mediumseagreen", "orange"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.08, -0.05))

plot(density(C.sum.df$chltot), col="tomato2", pch=16, cex=0.7, 
     xlab="Light (DLI)", ylab=expression(paste("chlorophyll", ~(mu*g~cm^-2), sep="")), 
     main="chl: Light and Season", ylim=c(0, 0.4), xlim=c(0,15), lwd=2)
lines(density(C.win.df$chltot), col="dodgerblue3", lwd=2)
lines(density(D.sum.df$chltot), col="orange", lwd=2)
lines(density(D.win.df$chltot), col="mediumseagreen", lwd=2)
legend("topright", c("C-sum", "C-win", "D-sum", "D-win"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3", "orange", "mediumseagreen"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.08, -0.05))


###### density plot: by season
plot(density(data.winter$chltot), ylim=c(0,0.3), xlim=c(0, 18), col="dodgerblue3", main="chl: Seasons", 
     xlab=expression(paste("chlorophyll", ~(mu*g~cm^-2), sep="")))
lines(density(data.summer$chltot), col="tomato2")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.35, -0.1))


###### density plot: by Site
plot(density(F1.R46.df$chltot), ylim=c(0,0.3), xlim=c(0, 18), col="tomato2", main="chl: Sites", 
     xlab=expression(paste("chlorophyll", ~(mu*g~cm^-2), sep="")), ylab="Density")
lines(density(R42.df$chltot), col="skyblue3")
lines(density(F8.R10.df$chltot), col="springgreen4")
lines(density(HIMB.df$chltot), col="purple")
legend("topright", c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))
dev.print(pdf, "figures/physiology/PanKB.chl.pdf", width=9, height=7)





##################
# Fig: chlorophyll/cell over season and depth
##################  
plot(chlcell~newDepth, data=data.winter,col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", 
     ylab=expression(paste("pg chlorophyll cell"^-1, sep="")), main="chl/cell: Depth and Season",
     xlim=c(0,10), ylim=c(0,15))
abline((lm(chlcell~newDepth, data=data.winter)), col='dodgerblue3', lwd=2)
points(chlcell~newDepth, data=data.summer, col="tomato2", pch=16, cex=0.7)
abline((lm(chlcell~newDepth, data=data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(data.winter$chlcell), ylim=c(0,0.4), xlim=c(0, 15), col="dodgerblue3", main="chl/cell: Seasons", xlab=expression(paste("pg chlorophyll cell"^-1, sep="")))
lines(density(data.summer$chlcell), col="tomato2")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(F1.R46.df$chlcell), ylim=c(0,0.4), xlim=c(0,15), col="tomato2", main="chl/cell: Sites", xlab=expression(paste("pg chlorophyll cell"^-1, sep="")))
lines(density(R42.df$chlcell), col="skyblue3")
lines(density(F8.R10.df$chlcell), col="springgreen4")
lines(density(HIMB.df$chlcell), col="purple")
legend("topright", c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/physiology/PanKB.chlcell.pdf", width=9, height=7)



##################
# Fig: symbionts over season and depth
##################        

plot((zoox/10^6)~newDepth, data=data.winter,col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab=(expression(paste(~italic("Symbiodinium") ~(10^6~cells~cm^-2), sep=""))), xlim=c(0,10), ylim=c(0, 6),
     main= "zoox: Depth and Season")
abline((lm((zoox/10^6)~newDepth, data=data.winter)), col='dodgerblue3', lwd=2)
points((zoox/10^6)~newDepth, data=data.summer, col="tomato2", pch=16, cex=0.7)
abline((lm((zoox/10^6)~newDepth, data=data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(data.summer$zoox/10^6), col="tomato2", main="zoox: Seasons", 
     xlab=(expression(paste(~italic("Symbiodinium") ~(10^6~cells~cm^-2), sep=""))), xlim=c(0, 7))
lines(density(data.winter$zoox/10^6), col="dodgerblue3")
legend('topright', c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(F1.R46.df$zoox/10^6), col="tomato2", main="zoox: Sites", 
     xlab=(expression(paste(~italic("Symbiodinium") ~(10^6~cells~cm^-2), sep=""))), xlim=c(0, 7))
lines(density(R42.df$zoox/10^6), col="skyblue3")
lines(density(F8.R10.df$zoox/10^6), col="springgreen4")
lines(density(HIMB.df$zoox/10^6), col="purple")
legend("topright", c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/physiology/PanKB.zoox.pdf", width=9, height=7)



##################
# Fig: biomass over season and depth
################## 

plot(biomass~newDepth, data=data.winter,col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste("Total biomass", ~(mg~cm^-2), sep="")),
     xlim=c(0,10), ylim=c(5,60), 
     main="biomass: Depth and Season")
abline((lm(biomass~newDepth, data=data.winter)), col='dodgerblue3', lwd=2)
points(biomass~newDepth, data=data.summer, col="tomato2", pch=16, cex=0.7)
abline((lm(biomass~newDepth, data=data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.08, -0.1))


###### density plot: by season
plot(density(data.winter$biomass), ylim=c(0,0.08), xlim=c(0, 70), col="dodgerblue3", main="biomass: Seasons", xlab=expression(paste("Total biomass", ~(mg~cm^-2), sep="")))
lines(density(data.summer$biomass), col="tomato2")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), cex=1, y.intersp = 0.3, x.intersp = 0.4, bty="n", inset=c(-0.35, -0.1))


###### density plot: by Site
plot(density(F1.R46.df$biomass), ylim=c(0,0.1), xlim=c(0,70), col="tomato2", main="biomass: Sites", 
     xlab=expression(paste("Total biomass", ~(mg~cm^-2), sep="")))
lines(density(R42.df$biomass), col="skyblue3")
lines(density(F8.R10.df$biomass), col="springgreen4")
lines(density(HIMB.df$biomass), col="purple")
legend("topright", c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), 
       y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/physiology/PanKB.biomass.pdf", width=9, height=7)




###############
### ISOTOPES ##
############### 

##################
# Fig: d13C host over season and depth
##################        

plot(host..d13C~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste(delta^{13}, C[H], " (\u2030, V-PDB)")),
     xlim=c(0,10), ylim=c(-20,-10),
     main= "d13C host: Depth and Season")
abline(lm(host..d13C~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(host..d13C~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(host..d13C~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$host..d13C)), col="tomato2", main="d13C-host: Seasons", 
     xlab=expression(paste(delta^{13}, C[H], " (\u2030, V-PDB)")), xlim=c(-20, -8))
lines(density(na.omit(data.winter$host..d13C)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$host..d13C)), col="tomato2", main="d13C-host: Sites", 
     xlab=expression(paste(delta^{13}, C[H], " (\u2030, V-PDB)")), ylim=c(0,0.4), xlim=c(-20, -8))
lines(density(na.omit(R42.df$host..d13C)), col="skyblue3")
lines(density(na.omit(F8.R10.df$host..d13C)), col="springgreen4")
lines(density(na.omit(HIMB.df$host..d13C)), col="purple")
legend("topright" ,c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.d13C-host.pdf", width=9, height=7, encod="MacRoman")



##################
# Fig: d13C symb over season and depth
##################        

plot(symb..d13C~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste(delta^{13}, C[S], " (\u2030, V-PDB)")),
     xlim=c(0,10), ylim=c(-20,-10),
     main= "d13C symb: Depth and Season")
abline(lm(symb..d13C~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(symb..d13C~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(symb..d13C~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$symb..d13C)), col="tomato2", main="d13C-symb: Seasons", 
     xlab=expression(paste(delta^{13}, C[S], " (\u2030, V-PDB)")), xlim=c(-20, -8))
lines(density(na.omit(data.winter$symb..d13C)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$symb..d13C)), col="tomato2", main="d13C-symb: Sites", 
     xlab=expression(paste(delta^{13}, C[S], " (\u2030, V-PDB)")), ylim=c(0,0.4), xlim=c(-20, -8))
lines(density(na.omit(R42.df$symb..d13C)), col="skyblue3")
lines(density(na.omit(F8.R10.df$symb..d13C)), col="springgreen4")
lines(density(na.omit(HIMB.df$symb..d13C)), col="purple")
legend("topright" ,c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.d13C-symb.pdf", width=9, height=7, encod="MacRoman")


##################
# Fig: d13C skeleton over season and depth
##################        

plot(d13C..skel~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste(delta^{13}, C[Skel], " (\u2030, V-PDB)")),
     xlim=c(0,10), ylim=c(-6,2),
     main= "d13C skel: Depth and Season")
abline(lm(d13C..skel~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(d13C..skel~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(d13C..skel~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$d13C..skel)), col="tomato2", main="d13C-skel: Seasons", 
     xlab=expression(paste(delta^{13}, C[Skel], " (\u2030, V-PDB)")), ylim=c(0, 0.5), xlim=c(-8, 6))
lines(density(na.omit(data.winter$d13C..skel)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$d13C..skel)), col="tomato2", main="d13C-skel: Sites", 
     xlab=expression(paste(delta^{13}, C[Skel], " (\u2030, V-PDB)")), ylim=c(0,0.5), xlim=c(-8, 6))
lines(density(na.omit(R42.df$d13C..skel)), col="skyblue3")
lines(density(na.omit(F8.R10.df$d13C..skel)), col="springgreen4")
lines(density(na.omit(HIMB.df$d13C..skel)), col="purple")
legend("topright", c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.d13C-skel.pdf", width=9, height=7, encod="MacRoman")


##################
# Fig: d13C host-symb over season and depth
##################        

plot(d13C..host.sym~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste(delta^{13}, C[H-S], " (\u2030, V-PDB)")), 
     xlim=c(0,10), ylim=c(-2.5,3),
     main= "d13C h-s: Depth and Season")
abline(lm(d13C..host.sym~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(d13C..host.sym~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(d13C..host.sym~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$d13C..host.sym)), col="tomato2", main="d13C h-s: Seasons", 
     xlab=expression(paste(delta^{13}, C[H-S], " (\u2030, V-PDB)")), ylim=c(0,1.5), xlim=c(-3, 5))
lines(density(na.omit(data.winter$d13C..host.sym)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$d13C..host.sym)), col="tomato2", main="d13C h-s: Sites", 
     expression(paste(delta^{13}, C[H-S], " (\u2030, V-PDB)")), ylim=c(0,1.7), xlim=c(-3,5))
lines(density(na.omit(R42.df$d13C..host.sym)), col="skyblue3")
lines(density(na.omit(F8.R10.df$d13C..host.sym)), col="springgreen4")
lines(density(na.omit(HIMB.df$d13C..host.sym)), col="purple")
legend("topright",c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.d13Ch-s.pdf", width=9, height=7, encod="MacRoman")




##################
# Fig: d15N host over season and depth
##################        

plot(host..d15N~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste(delta^{15}, N[H], " (\u2030, air)")), xlim=c(0,10), ylim=c(2,8),
     main= "d15N host: Depth and Season")
abline(lm(host..d15N~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(host..d15N~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(host..d15N~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$host..d15N)), col="tomato2", main="d15N-host: Seasons", 
     xlab=expression(paste(delta^{15}, N[H], " (\u2030, air)")), xlim=c(0, 12), ylim=c(0, 1))
lines(density(na.omit(data.winter$host..d15N)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$host..d15N)), col="tomato2", main="d15N-host: Sites", xlab=expression(paste(delta^{15}, N[H], " (\u2030, air)")), ylim=c(0,2), xlim=c(0, 12))
lines(density(na.omit(R42.df$host..d15N)), col="skyblue3")
lines(density(na.omit(F8.R10.df$host..d15N)), col="springgreen4")
lines(density(na.omit(HIMB.df$host..d15N)), col="purple")
legend("topright" ,c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.d15N-host.pdf", width=9, height=7, encod="MacRoman")




##################
# Fig: d15N symb over season and depth
##################        

plot(symb..d15N~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab =expression(paste(delta^{15}, N[S], " (\u2030, air)")),
     xlim=c(0,10), ylim=c(2,7),
     main= "d15N symb: Depth and Season")
abline(lm(symb..d15N~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(symb..d15N~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(symb..d15N~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$symb..d15N)), col="tomato2", main="d15N-symb: Seasons", 
     xlab= expression(paste(delta^{15}, N[S], " (\u2030, air)")), xlim=c(0, 9), ylim=c(0, 0.9))
lines(density(na.omit(data.winter$symb..d15N)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$symb..d15N)), col="tomato2", main="d15N-symb: Sites", 
     xlab= expression(paste(delta^{15}, N[S], " (\u2030, air)")),  xlim=c(0, 9), ylim=c(0,1.8))
lines(density(na.omit(R42.df$symb..d15N)), col="skyblue3")
lines(density(na.omit(F8.R10.df$symb..d15N)), col="springgreen4")
lines(density(na.omit(HIMB.df$symb..d15N)), col="purple")
legend("topright" ,c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.d15N-symb.pdf", width=9, height=7, encod="MacRoman")



##################
# Fig: d15N host.symb over season and depth
##################        

plot(d15N..host.sym~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste(delta^{15}, N[H-S], " (\u2030, air)")),
     xlim=c(0,10), ylim=c(-1,2),
     main= "d15N h-s: Depth and Season")
abline(lm(d15N..host.sym~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(d15N..host.sym~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(d15N..host.sym~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$d15N..host.sym)), col="tomato2", main="d15N h-s: Seasons", 
     xlab=expression(paste(delta^{15}, N[H-S], " (\u2030, air)")), xlim=c(-1, 3), ylim=c(0,2))
lines(density(na.omit(data.winter$d15N..host.sym)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.3, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$d15N..host.sym)), col="tomato2", main="d15N h-s: Sites", xlab=expression(paste(delta^{15}, N[H-S], " (\u2030, air)")), ylim=c(0,2), xlim=c(-1,3))
lines(density(na.omit(R42.df$d15N..host.sym)), col="skyblue3")
lines(density(na.omit(F8.R10.df$d15N..host.sym)), col="springgreen4")
lines(density(na.omit(HIMB.df$d15N..host.sym)), col="purple")
legend("topright" ,c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.d15Nh-s.pdf", width=9, height=7, encod="MacRoman")



##################
# Fig: C.N host season and depth
##################       
plot(host..C.N~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste("C:N"[H])),
     xlim=c(0,10), ylim=c(5,10),
     main= "C:N-host Depth and Season")
abline(lm(host..C.N~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(host..C.N~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(host..C.N~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$host..C.N)), col="tomato2", main="C:N-host Depth and Season", 
     xlab=expression(paste("C:N"[H])), ylim=c(0, 1), xlim=c(4, 10))
lines(density(na.omit(data.winter$host..C.N)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$host..C.N)), col="tomato2", main="C:N-host Depth and Season", 
     xlab=expression(paste("C:N"[H])), ylim=c(0,1.5), xlim=c(4, 10))
lines(density(na.omit(R42.df$host..C.N)), col="skyblue3")
lines(density(na.omit(F8.R10.df$host..C.N)), col="springgreen4")
lines(density(na.omit(HIMB.df$host..C.N)), col="purple")
legend("topright", c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.C.Nhost.pdf", width=9, height=7, encod="MacRoman")



##################
# Fig: C.N symb season and depth
##################       
plot(symb..C.N~newDepth, data=na.omit(data.winter),col="dodgerblue3", pch=16, cex=0.7, 
     xlab="Depth (m)", ylab = expression(paste("C:N"[S])),
     xlim=c(0,10), ylim=c(5,14),
     main= "C:N-symb Depth and Season")
abline(lm(symb..C.N~newDepth, data=na.omit(data.winter)), col='dodgerblue3', lwd=2)
points(symb..C.N~newDepth, data=na.omit(data.summer), col="tomato2", pch=16, cex=0.7)
abline(lm(symb..C.N~newDepth, data=na.omit(data.summer)), col='tomato2', lwd=2)
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.08, -0.1))

###### density plot: by season
plot(density(na.omit(data.summer$symb..C.N)), col="tomato2", main="C:N-symb Depth and Season", 
     xlab=expression(paste("C:N"[S])), ylim=c(0, 0.8), xlim=c(5, 15))
lines(density(na.omit(data.winter$symb..C.N)), col="dodgerblue3")
legend("topright", c("summer", "winter"), lty=c(1,1), lwd=c(2,2), 
       col=c("tomato2", "dodgerblue3"), y.intersp = 0.3, x.intersp = 0.4, cex=1, bty="n", inset=c(-0.35, -0.1))

###### density plot: by Site
plot(density(na.omit(F1.R46.df$symb..C.N)), col="tomato2", main="C:N-symb Depth and Season", 
     xlab=expression(paste("C:N"[S])), ylim=c(0,0.8), xlim=c(5, 15))
lines(density(na.omit(R42.df$symb..C.N)), col="skyblue3")
lines(density(na.omit(F8.R10.df$symb..C.N)), col="springgreen4")
lines(density(na.omit(HIMB.df$symb..C.N)), col="purple")
legend("topright", c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"), lty=c(1,1), lwd=c(2,2), y.intersp = 0.3, x.intersp = 0.4,
       col=c("sienna1", "skyblue3", "springgreen4", "purple"), cex=0.8, bty='n', inset=c(-0.5, -0.1))

dev.print(pdf, "figures/isotope/PanKB.C.Nsym.pdf", width=9, height=7, encod="MacRoman")



##################################
## means for area normalized df ##
##################################
biomass.m<-aggregate(biomass~Season+Location, data, mean); biomass.m
zoox.m<-aggregate(zoox~Season+Location, data, mean); zoox.m
chla.m<-aggregate(chla~Season+Location, data, mean); chla.m
chlc2.m<-aggregate(chlc2~Season+Location, data, mean); chlc2.m
chlacell.m<-aggregate(chlacell~Season+Location, data, mean); chlacell.m
chlc2cell.m<-aggregate(chlc2cell~Season+Location, data, mean); chlc2cell.m

# summary dataframe
df.m<-cbind(biomass.m, zoox.m[c(3,0)], chla.m[c(3,0)], chlc2.m[c(3,0)], chlacell.m[c(3,0)], chlc2cell.m[c(3,0)])
df.m

#####################################
# SE
#####################################
biomass.SE<-aggregate(biomass~Season+Location, data, std.error); biomass.SE
zoox.SE<-aggregate(zoox~Season+Location, data, std.error); zoox.SE
chla.SE<-aggregate(chla~Season+Location, data, std.error); chla.SE
chlc2.SE<-aggregate(chlc2~Season+Location, data, std.error); chlc2.SE
chlacell.SE<-aggregate(chlacell~Season+Location, data, std.error); chlacell.SE
chlc2cell.SE<-aggregate(chlc2cell~Season+Location, data, std.error); chlc2cell.SE


df.SE<-cbind(biomass.SE, zoox.SE[c(3,0)], chla.SE[c(3,0)], chlc2.SE[c(3,0)], chlacell.SE[c(3,0)], chlc2cell.SE[c(3,0)])
df.SE

#####################################
# sample size
#####################################
biomass.n<-aggregate(biomass~Season+Location, data, length); biomass.n
zoox.n<-aggregate(zoox~Season+Location, data, length); zoox.n
chla.n<-aggregate(chla~Season+Location, data, length); chla.n
chlc2.n<-aggregate(chlc2~Season+Location, data, length); chlc2.n
chlacell.n<-aggregate(chlacell~Season+Location, data, length); chlacell.n
chlc2cell.n<-aggregate(chlc2cell~Season+Location, data, length); chlc2cell.n

df.n<-cbind(biomass.n, zoox.n[c(3,0)], chla.n[c(3,0)], chlc2.n[c(3,0)], chlacell.n[c(3,0)], chlc2cell.n[c(3,0)])
df.n

##### create dfframe with means and SE
df<-cbind(df.m, df.SE[c(3:8,0)], df.n[c(3,0)]); colnames(df) <-c("Season", "Location", "biomass", "zoox", "chla", "chlc2", "chlacell", "chlc2cell", "biomassSE", "zooxSE", "chlaSE", "chlc2SE","chlacellSE", "chlc2cellSE", "sample size")


#####################################
pd <- position_dodge(0.1) #offset for error bars
#####################################
#formatting
format<-theme_classic() +
  theme(text=element_text(size=12), axis.line=element_blank())+
  theme(axis.title.x = element_text(face="bold", size=14))+
  theme(panel.background = element_rect(colour = "black", size=1))+ theme(aspect.ratio=1)+
  theme(axis.ticks.length=unit(-0.25, "cm"), 
        axis.text.y=element_text(margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm")), 
        axis.text.x=element_text(margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm")))

#####################################
# AFDW (biomass) normalized to area (mgdw/cm2)
Fig.AFDW<-ggplot(data=df, aes(x=df$Season, y=df$biomass, fill=Location, group=Location)) + geom_errorbar(aes(ymin=df$biomass-df$biomassSE, ymax=df$biomass+df$biomassSE),size=.5, width=0, position=pd) +
  geom_line(position=pd, size=.5) +
  geom_point(aes(fill=Location), position=pd, size=4, pch=21) +
  xlab("Season") +
  scale_fill_discrete(name="Reef Sites",
                      breaks=c("F1-R46", "R42", "F8-R10", "HIMB"),
                      labels=c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"))+
  ylab(expression(paste("Biomass" ~(mg~cm^-2), sep=""))) +
  scale_y_continuous(limits=c(10, 45)) + format; Fig.AFDW
ggsave(file="figures/physiology/Fig.AFDW.pdf")


#####################################
# symbiont densities (symbionts/cm2)
Fig.symb<-ggplot(data=df, aes(x=df$Season, y=df$zoox/10^6, group=df$Location, fill=Location)) + geom_errorbar(aes(ymin=df$zoox/10^6-df$zooxSE/10^6, ymax=df$zoox/10^6+df$zooxSE/10^6),size=.5, width=0, position=pd) +
  geom_line(position=pd, size=.5) +
  geom_point(aes(fill=Location), position=pd, size=5, pch=21) +
  xlab("Season") +
  scale_fill_discrete(name="Reef Sites",
                      breaks=c("F1-R46", "R42", "F8-R10", "HIMB"),
                      labels=c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"))+
  ylab(expression(paste(~italic("Symbiodinium")~(10^6~cells~cm^-2), sep=""))) +
  scale_y_continuous(limits=c(0.5, 3.5)) + format; Fig.symb
ggsave(file="figures/physiology/Fig.symb.pdf")

#####################################
##### chlorophyll a (ug/cm2)
Fig.chla<-ggplot(data=df, aes(x=df$Season, y=df$chla, group=df$Location, fill=Location)) + geom_errorbar(aes(ymin=df$chla-df$chlaSE, ymax=df$chla+df$chlaSE),size=.5, width=0, position=pd) +
  geom_line(position=pd, size=.5) +
  geom_point(aes(fill=Location), position=pd, size=5, pch=21) +
  xlab("Season") +
  scale_fill_discrete(name="Reef Sites",
                      breaks=c("F1-R46", "R42", "F8-R10", "HIMB"),
                      labels=c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"))+
  scale_y_continuous(limits=c(4, 12)) +
  ylab(expression(paste("Chlorophyll", ~italic("a"), ~(mu*g~cm^-2), sep=""))) + 
  format; Fig.chla
ggsave(file="figures/physiology/Fig.chla.pdf")

#####################################
# chlorophyll c2 concentration (ug chl c2/cm2)
Fig.chlc<-ggplot(data=df, aes(x=df$Season, y=df$chlc2, group=df$Location, fill=Location)) + 
  geom_errorbar(aes(ymin=df$chlc2-df$chlc2SE, ymax=df$chlc2+df$chlc2SE),size=.5, width=0, position=pd) +
  geom_line(position=pd, size=.5) +
  geom_point(aes(fill=Location), position=pd, size=5, pch=21) +
  xlab("Season") +
  scale_fill_discrete(name="Reef Sites",
                      breaks=c("F1-R46", "R42", "F8-R10", "HIMB"),
                      labels=c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"))+
  scale_y_continuous(limits=c(0.5, 3)) +
  ylab(expression(paste("Chlorophyll", ~italic("c"[2]), ~(mu*g~cm^-2), sep=""))) +
  format; Fig.chlc
ggsave(file="figures/physiology/Fig.chlc.pdf")


#####################################
# chlorophyll a per symbiont cell (pg chla/cell)
Fig.chlacell<-ggplot(data=df, aes(x=df$Season, y=df$chlacell, group=df$Location, fill=Location)) + geom_errorbar(aes(ymin=df$chlacell-df$chlacellSE, ymax=df$chlacell+df$chlacellSE),size=.5, width=0, position=pd) +
  geom_line(position=pd, size=.5) +
  geom_point(aes(fill=Location), position=pd, size=5, pch=21) +
  xlab("Season") +
  scale_fill_discrete(name="Reef Sites",
                      breaks=c("F1-R46", "R42", "F8-R10", "HIMB"),
                      labels=c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"))+
  ylab(expression(paste("Chlorophyll", ~italic("a"), ~(pg~cell^-1), sep=""))) +
  scale_y_continuous(limits=c(2, 6)) +
  format; Fig.chlacell
ggsave(file="figures/physiology/Fig.chlacell.pdf")


#####################################
# chlorophyll c2 per symbiont cell (pg chl c2/cell)
Fig.chlc2cell<-ggplot(data=df, aes(x=df$Season, y=df$chlc2cell, group=df$Location, fill=Location)) + geom_errorbar(aes(ymin=df$chlc2cell-df$chlc2cellSE, ymax=df$chlc2cell+df$chlc2cellSE),size=.5, width=0, position=pd) +
  geom_line(position=pd, size=.5) +
  geom_point(aes(fill=Location), position=pd, size=5, pch=21) +
  xlab("Season") +
  scale_fill_discrete(name="Reef Sites",
                      breaks=c("F1-R46", "R42", "F8-R10", "HIMB"),
                      labels=c("Fringe-Reef 46", "Patch-Reef 42", "Fringe-Reef 10", "HIMB"))+
  ylab(expression(paste("Chlorophyll", ~italic("c"[2]), ~(pg~cell^-1), sep=""))) +
  scale_y_continuous(limits=c(0.5, 2)) +
  format; Fig.chlc2cell 
ggsave(file="figures/physiology/Fig.chlccell.pdf")

####################

