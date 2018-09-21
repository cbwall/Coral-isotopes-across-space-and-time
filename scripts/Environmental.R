###############################
### Environmental data
###############################
rm(list=ls())

library(ggplot2)
library(plyr)
library(plotrix)
library(lubridate)
library(effects)
library(RCurl)
library(chron)
library(lubridate)
library(lme4)
library(lsmeans)
library(multcomp)
library(devtools)
library(logihist)
library(popbio)
library(car)

# DLI
##### all DLI for graphs
files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/all DLI", pattern = "csv$", full.names = T)
tables <- lapply(files, read.csv, header = TRUE)
DLI<-do.call(rbind, tables)
DLI=DLI[-1] # remove junk column
DLI$Date<-as.Date(DLI$Date) # fix date
DLI<-DLI[order(DLI$Date),] # order by Date

# make a date sequence for entire study
all.date<-as.data.frame(seq(as.Date("2016-06-10"), as.Date("2017-01-12"), "days"))
colnames(all.date)="Date"

# merge the DLI data and the date sequence to make a complete df through time
DLI.3site<-merge(all.date, DLI, by="Date", all.x=T)
R10<-read.csv("data/environmental/temp and light/Jun_DecPAR/all DLI/Reef 10/DLI.Rf102016.csv")
R10<-R10[-1]; R10$Date<-as.Date(R10$Date)

DLI.4site<-merge(DLI.3site, R10, by="Date", all.x=T)
DLI.4site$month <- months(as.Date(DLI.4site$Date)) # makes a month column
DLI.4site<-DLI.4site[, c(1,6, 2:5)]

# determine monthly mean for PAR during deployments at depth
write.csv(DLI.4site, "data/environmental/temp and light/Jun_DecPAR/All.DLI.csv")


##### Figure
All.DLI<-DLI.4site
# All.DLI <- read.csv("data/environmental/temp and light/Jun_DecPAR/All.DLI.csv")[,-1] # DLI at mid-depth
# All.DLI$Date<-as.Date(All.DLI$Date)

reefcols=c("mediumseagreen", "dodgerblue", "salmon", "orchid")
par(mar=c(2,3.6,1,1.3), mgp=c(2,0.5,0))
layout(matrix(c(1,2,3,4), 2,2, byrow=TRUE))

plot(Rf44.mid.DLI~Date, type="l", All.DLI, ylab=(expression(paste("DLI (mol photons", ~m^-2, ~d^-1,")", sep=""))), ylim=c(0, 30), xaxt="n", xlab="Time", col=reefcols[1])
legend("topright", lty=1, col=reefcols[1], legend="Reef 44", lwd=1.5, bty="n", cex=0.8, 
       x.intersp = 0.3, inset=c(-0.1, -0.05))
axis.Date(1, at=seq(min(All.DLI$Date), max(All.DLI$Date), by="2 mo"), format="%b '%y")
abline(l)

plot(Rf42.mid.DLI~Date, type="l", All.DLI, ylab=(expression(paste("DLI (mol photons", ~m^-2, ~d^-1,")", sep=""))), ylim=c(0, 30), xaxt="n", xlab="Time", col=reefcols[2])
legend("topright", lty=1, col=reefcols[2], legend="Reef 42", lwd=1.5, bty="n", cex=0.8, 
       x.intersp = 0.3, inset=c(-0.1, -0.05))
axis.Date(1, at=seq(min(All.DLI$Date), max(All.DLI$Date), by="2 mo"), format="%b '%y")

plot(Rf10.mid.DLI~Date, type="l", All.DLI, ylab=(expression(paste("DLI (mol photons", ~m^-2, ~d^-1, ")", sep=""))), ylim=c(0, 30), xaxt="n", xlab="Time", col=reefcols[3])
legend("topright", lty=1, col=reefcols[3], legend="Reef 10", lwd=1.5, bty="n", cex=0.8, 
       x.intersp = 0.3, inset=c(-0.1, -0.05))
axis.Date(1, at=seq(min(All.DLI$Date), max(All.DLI$Date), by="2 mo"), format="%b '%y")

plot(HIMB.mid.DLI~Date, type="l", All.DLI, ylab=(expression(paste("DLI (mol photons", ~m^-2, ~d^-1, ")", sep=""))), ylim=c(0, 30), xaxt="n", xlab="Time", col=reefcols[4])
legend("topright", lty=1, col=reefcols[4], legend="HIMB", lwd=1.5, bty="n", cex=0.8, 
       x.intersp = 0.3, inset=c(-0.1, -0.05))
axis.Date(1, at=seq(min(All.DLI$Date), max(All.DLI$Date), by="2 mo"), format="%b '%y")

dev.copy(pdf, "figures/environmental/All.DLI.pdf", width=6.5, height=4)
dev.off()


#########################
#########################
# nutrients
nutr<-read.csv("data/environmental/PanKBay_nutrients.csv")

#models
mod<-lm(phosphate~Location+Date, data=nutr); Anova(mod, type=2)
mod<-lm(silicate~Location+Date, data=nutr); Anova(mod, type=2)
mod<-lm(N.N~Location+Date, data=nutr); Anova(mod, type=2)
mod<-lm(ammonium~Location+Date, data=nutr); Anova(mod, type=2)
#OR.... do t-tests at each date in separate period-dataframes

nutr$Date<-mdy(nutr$Date) # corrects date format
dates<-cbind("10-Aug '16", "20-Dec '16")

RfHIMB<-nutr[(nutr$Location=="HIMB"), ]
Rf42<-nutr[(nutr$Location=="R42"), ]
Rf46<-nutr[(nutr$Location=="F1-46"), ]
RF10<-nutr[(nutr$Location=="F8-R10"), ]

Reefs<-c("Reef 46", "Reef 42", "Reef 10", "HIMB")
plot_colors<-c("mediumseagreen", "dodgerblue", "salmon", "orchid")

###############
# Phosphate
# par(mfrow=c(1,1), mar=c(2,4,1,1), mgp=c(2,0.5,0))

# figure layout
layout(matrix(c(1,2,3,4), nrow=1, byrow=TRUE))
par(mar=c(5,3.5,3,1))

plot(y=Rf46$phosphate, x=Rf46$Date, xaxt="n", type="o", xlab=NA, ylab=expression(paste("phosphate"~(mu*mol~L^-1), sep="")), ylim=c(0, 0.25), pch=19, lty=2, cex=1, lwd=1, col=plot_colors[1], cex.axis=0.8)
axis(side=1, at=Rf46$Date, labels=dates, cex.axis=0.8) # plots HIMB
#plots Reef 42
with(Rf46, lines(Rf42$phosphate, x=Rf42$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[2]))
#plots HIMB
with(RfHIMB, lines(RfHIMB$phosphate, x=RfHIMB$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[3]))
#plots Reef 10
with(Rf46, lines(RF10$phosphate, x=RF10$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[4]))

##### save the figure and export to directory? ####
#dev.copy(pdf, "figures/environmental/phosphate.pdf", encod="MacRoman", height=4, width=5)
#dev.off()

###############
# Silicate
plot(y=Rf46$silicate, x=Rf46$Date, xaxt="n", type="o", xlab=NA, ylab=expression(paste("silicate"~(mu*mol~L^-1), sep="")), ylim=c(0, 15), pch=19, lty=2, cex=1, lwd=1, col=plot_colors[1], cex.axis=0.8)
axis(side=1, at=Rf46$Date, labels=dates, cex.axis=0.8) # plots HIMB
#plots Reef 42
with(Rf46, lines(Rf42$silicate, x=Rf42$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[2]))
#plots HIMB
with(RfHIMB, lines(RfHIMB$silicate, x=RfHIMB$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[3]))
#plots Reef 10
with(Rf46, lines(RF10$silicate, x=RF10$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[4]))

##### save the figure and export to directory? ####
#dev.copy(pdf, "figures/environmental/silicate.pdf", encod="MacRoman", height=4, width=5)
#dev.off()

###############
# N+N
plot(y=Rf46$N.N, x=Rf46$Date, xaxt="n", type="o", xlab=NA, ylab=expression(paste("nitrate+nitrite"~(mu*mol~L^-1), sep="")), ylim=c(0, 1.5), pch=19, lty=2, cex=1, lwd=1, col=plot_colors[1], cex.axis=0.8)
axis(side=1, at=Rf46$Date, labels=dates, cex.axis=0.8) # plots HIMB
#plots Reef 42
with(Rf46, lines(Rf42$N.N, x=Rf42$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[2]))
#plots HIMB
with(RfHIMB, lines(RfHIMB$N.N, x=RfHIMB$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[3]))
#plots Reef 10
with(Rf46, lines(RF10$N.N, x=RF10$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[4]))

##### save the figure and export to directory? ####
#dev.copy(pdf, "figures/environmental/N.N.pdf", encod="MacRoman", height=4, width=5)
#dev.off()

###############
# ammonium
plot(y=Rf46$ammonium, x=Rf46$Date, xaxt="n", type="o", xlab=NA, ylab=expression(paste("ammonium"~(mu*mol~L^-1), sep="")), ylim=c(0, 1.5), pch=19, lty=2, cex=1, lwd=1, col=plot_colors[1], cex.axis=0.8)
axis(side=1, at=Rf46$Date, labels=dates, cex.axis=0.8) # plots HIMB
#plots Reef 42
with(Rf46, lines(Rf42$ammonium, x=Rf42$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[2]))
#plots HIMB
with(RfHIMB, lines(RfHIMB$ammonium, x=RfHIMB$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[3]))
#plots Reef 10
with(Rf46, lines(RF10$ammonium, x=RF10$Date, type="o", pch=19, lty=2, cex=1, lwd=1, col=plot_colors[4]))
legend("topleft", legend=Reefs, col=plot_colors[1:4], lwd=1, pch=19, lty=2, cex=0.8, bty="n", x.intersp=0.2, y.intersp=1.2, inset=c(0.45, 0), seg.len=0.9)

##### save the figure and export to directory? ####
#dev.copy(pdf, "figures/environmental/ammonium.pdf", encod="MacRoman", height=4, width=5)
#dev.off()

dev.copy(pdf, "figures/environmental/all.nutrients.pdf", encod="MacRoman", height=3, width=7)
dev.off()



#########################
#########################
# SPM
par(mfrow=c(1,1))
spm<-read.csv("data/environmental/PanKBay_SPM.csv")
spm$SPM..g.l<-spm$SPM..g.l*1000 # change to mg
colnames(spm)[3]="SPM.mg"

mean<-aggregate(SPM.mg~Date + Reef.ID, spm, mean)
SE<-aggregate(SPM.mg~Date+Reef.ID, spm, std.error)

spm.df<-cbind(mean, SE[c(0,3)])
colnames(spm.df)=c("Date", "Location", "SPM", "SE")
spm.df$Date<-ordered(spm.df$Date, c("8/10/16", "12/20/16"))
spm.df$Location<-ordered(spm.df$Location, c("F1-46", "R42", "F8-10", "HIMB"))

Reefs<-c("Reef 46", "Reef 42", "Reef 10", "HIMB")
Date<-c("10-Aug '16", "20-Dec '16")
#####################################

# figure formatting script
Fig.formatting<-(theme_classic()) +
  theme(text=element_text(size=10),
        axis.line=element_blank(),
        legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect("transparent", colour=NA),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        aspect.ratio=1, 
        axis.ticks.length=unit(0.25, "cm"),
        axis.text.y=element_text(
          margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=10), 
        axis.text.x=element_text(
          margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=10)) +
  theme(legend.key.size = unit(0.4, "cm")) 

pd <- position_dodge(0.71) #offset for error bars and columns

# SPM (mg/l)
Fig.SPM<-ggplot(spm.df, aes(x=Date, y=SPM, fill=Location)) +
  geom_bar(colour="black", stat="identity", position = pd, width=0.7) +
  geom_errorbar(aes(ymin=SPM-SE, ymax=SPM+SE),size=.5, width=0, position=pd) +
  xlab("Sampling points") +
  ylab(expression(paste("SPM", ~(mg~L^-1), sep=""))) +
  scale_x_discrete(labels=Date) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 5)) +
  scale_fill_manual(values=c("gray92", "gray72", "gray50", "gray10"),
                    labels=Reefs)+ Fig.formatting; Fig.SPM
ggsave(file="figures/environmental/SPM.pdf")
