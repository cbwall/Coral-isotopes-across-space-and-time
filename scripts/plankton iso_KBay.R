data<-read.csv("~/Desktop/Research and Teaching/UH MANOA/Research/Isotopes pan KBay/R/data/isotopes_SW_all times.csv")

str(data)

winter.data<-data[(data$Time.point=="winter"),]
summer.data<-data[(data$Time.point=="summer"),]

op<-par(mfrow = c(2,2), mar=c(5,4,1,2))
plot(d13C~SW.fraction..um+Reef.ID, data=winter.data, ylim=c(-25,-10), main="winter")
plot(d13C~SW.fraction..um+Reef.ID, data=summer.data, ylim=c(-25,-10), main="summer")

plot(d15N~SW.fraction..um+Reef.ID, data=data, ylim=c(4,9), main="across seasons")

plot((ug.C/ug.N)~SW.fraction..um+Reef.ID, data=winter.data, ylim=c(3,13), main="winter")
plot((ug.C/ug.N)~SW.fraction..um+Reef.ID, data=summer.data, ylim=c(3,13), main="summer")
plot((ug.C/ug.N)~Site, data=summer.data, ylim=c(3,13), main="all sites, seasons")

mix.N.mean<-aggregate(d15N~SW.fraction..um, data=data, mean)
mix.N.SE<-aggregate(d15N~SW.fraction..um, data=data, std.error)
mix.C.mean<-aggregate(d13C~SW.fraction..um, data=data, mean)
mix.C.SE<-aggregate(d13C~SW.fraction..um, data=data, std.error)
mix.data<-cbind(mix.N.mean, mix.C.mean[c(2,0)], mix.N.SE[c(2,0)], mix.C.SE[c(2,0)]); colnames(mix.data)=c("fraction", "d15N", "d13C", "d15N.SE", "d13C.SE"); mix.data

colors=c("#FF6A6A", "#00B2EE", "#3CB371", "#FFB90F", "#8B7500")
op<-par(mfrow = c(1,1), mar=c(5,4,1,5),xpd=TRUE, pty="sq")
levels=c("<243 um", ">243 um", "0-10 um", "10-100 um", "100-243 um")

plot(d15N~d13C, data=mix.data, xlim=c(-23,-17), yaxt="n", ylab="", ylim=c(4.5,8), pch=19, cex=2, col=colors, xlab=expression(paste(delta^{13}, "C (‰, v-PDB)")))
axis(4)
mtext(expression(paste(delta^{15}, "N (‰, air)")), side=4, line =3)
legend("topleft", inset=c(-0.05,-0.05), legend=levels, col=colors, pch=19, cex=1, bty="n", x.intersp=0.4, y.intersp = 0.35)
arrows(mix.data$d13C-mix.data$d13C.SE, mix.data$d15N, mix.data$d13C+mix.data$d13C.SE, mix.data$d15N, length=0.05, angle=90, code=3)
arrows(mix.data$d13C, mix.data$d15N-mix.data$d15N.SE, mix.data$d13C, mix.data$d15N+mix.data$d15N.SE, length=0.05, angle=90, code=3)

## To  move the labels you set ann=FALSE or xlab="", ylab="" and add them afterwards with mtext, where side=1 is bottom, 2 is left, 3 is top, 4 is right. line controls the distance from the plot area

dev.copy(pdf, "~/Desktop/Research and Teaching/UH MANOA/Research/Isotopes pan KBay/R/output/iso.sources.KBay.pdf", encod="MacRoman")
dev.off()

