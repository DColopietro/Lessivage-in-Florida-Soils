data=data.frame(read.csv("SandSizeDistribution.csv",header=TRUE))
str(data)

sort(unique(data$ID))

tiff(filename="SandSizeDist_1.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 1:70){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_2.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 71:140){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_3.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 141:210){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_4.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 211:280){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_5.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 281:350){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_6.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 351:420){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_7.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 421:490){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_8.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 491:560){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_9.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 561:630){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_10.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 631:700){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
dev.off()

tiff(filename="SandSizeDist_11.tiff",width=10,height=7.5,units="in",res=300)
par(oma=c(0.1,0.1,3.5,0.1),mar=c(1,3,0.7,1),mfrow=c(7,10),xpd=NA)
z=unique(data$ID)
for (i in 701:734){
	a=subset(data,data$ID==z[i])
	b=a[,c(9:13)]
	b=apply(b,2,rev)
	barplot(t(b),horiz=T,beside=F,las=1,col=c("red","orange","yellow","olivedrab","navy"),
		axes=F,names.arg=rev(a$UI),main=z[i])
	axis(1,at=seq(0,100,20),labels=c("","","","","",""))
}
legend(140,10,ncol=1,c("1-2mm","0.5-1mm","0.25-0.5mm","0.1-0.25mm","0.05-0.1mm"),
	fill=c("navy","olivedrab","yellow","orange","red"))
dev.off()
