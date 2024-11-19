#read in data for the positive FSSC profile horizons after profiles with UI>0.8 have been removed 
Horizons=data.frame(read.csv("Horizon Database.csv",header=TRUE))
str(Horizons)

#for loops to identify the thickness of zones of accumulation (horizons below "top") in each profile
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 
	
	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,5,7)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<-as.numeric(a[,2])						#converting factors to numeric values
	
	Zgain=sum(a$Z)							 	#sum all the horizons in the zone of accumulation to get a total thickness

	b=subset(temp,temp$ZONE=="loss")[,c(2,5,7)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<-as.numeric(b[,2])						#converting factors to numeric values

	Zloss=sum(b$Z)								#sum all the horizons in the zone of loss to get a total thickness
	
	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-Zloss
	temp2[i,3]<-Zgain

	}
colnames(temp2)<-c("ID","Zloss","Zgain")
Thickness=temp2
View(Thickness)

#SAVE data
write.csv(Thickness,"FSSC_Thickness of Loss and Accumulation.csv",row.names=F)


#for loops to calculate the relative contribution of each horizon to the zone of loss 
holder=data.frame()
profiles=(unique(Horizons$ID))
for(i in 1:length(profiles)){ 
	
	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="loss")[,c(1,5,7)]			#Selecting horizons from each profile identified as being in the zone of loss	

	a[,4]=a[,2]/sum(a$Z)	

	temp2=a[,c(1,4)]	
	holder=rbind(holder,temp2)						#hold and compile modified records here
}

Weightedcontribution=holder
View(Weightedcontribution)

#SAVE data
write.csv(Weightedcontribution,"FSSC_WeightedcontributionLoss.csv",row.names=F)


#for loops to calculate the relative contribution of each horizon to the zone of accumulation
holder=data.frame()
profiles=(unique(Horizons$ID))
for(i in 1:length(profiles)){ 
	
	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(1,5,7)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
		
	a[,4]=a[,2]/sum(a$Z)	

	temp2=a[,c(1,4)]	
	holder=rbind(holder,temp2)						#hold and compile modified records here
}

Weightedcontribution=holder
View(Weightedcontribution)

#SAVE data
write.csv(Weightedcontribution,"FSSC_WeightedcontributionGain.csv",row.names=F)


#read in data for the positive FSSC profile horizons after weighted contributions column was added 
Horizons=data.frame(read.csv("Horizon Database_1.csv",header=TRUE))
str(Horizons)


#for loops to calculate the thickness weighted average of clay concentration in zones of loss and accumulation
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(1,2,6,12)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,3]<- as.numeric(as.character(a[,3]))				#converting factors to numeric values

	averageC=a[,3]*a[,4]
	I=sum(averageC)

	b=subset(temp,temp$ZONE=="loss")[,c(1,2,6,12)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,3]<- as.numeric(as.character(b[,3]))				#converting factors to numeric values

	averageC=b[,3]*b[,4]
	E=sum(averageC)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E
	temp2[i,3]<-I

}	

colnames(temp2)<-c("ID","E","I")

FSSC_IE_ratio=temp2
View(FSSC_IE_ratio)

FSSC_IE_ratio$IE=0
View(FSSC_IE_ratio)

#Calculating the IE ratio
FSSC_IE_ratio$IE=FSSC_IE_ratio[,3]/FSSC_IE_ratio[,2]
View(FSSC_IE_ratio)

#SAVE data
write.csv(FSSC_IE_ratio,"FSSC_IE_Ratio.csv",row.names=F)


#for loops to calculate the average thickness weighted sand content in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,10)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	averageSand=a[,2]*a[,3]
	I_Sand=sum(averageSand)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,10)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	averageSand=b[,2]*b[,3]
	E_Sand=sum(averageSand)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_Sand
	temp2[i,3]<-I_Sand

}

colnames(temp2)<-c("ID","E_Sand","I_Sand")
View(temp2)

FSSC_sand=temp2
View(FSSC_sand)


#for loops to calculate the average thickness weighted silt content in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,11)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	averageSilt=a[,2]*a[,3]
	I_Silt=sum(averageSilt)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,11)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	averageSilt=b[,2]*b[,3]
	E_Silt=sum(averageSilt)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_Silt
	temp2[i,3]<-I_Silt

}

colnames(temp2)<-c("ID","E_Silt","I_Silt")
View(temp2)

FSSC_silt=temp2
View(FSSC_silt)

#for loops to calculate the average thickness weighted clay content in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,12)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	averageClay=a[,2]*a[,3]
	I_Clay=sum(averageClay)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,12)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	averageClay=b[,2]*b[,3]
	E_Clay=sum(averageClay)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_Clay
	temp2[i,3]<-I_Clay

}

colnames(temp2)<-c("ID","E_Clay","I_Clay")
View(temp2)

FSSC_clay=temp2
View(FSSC_clay)


#SAVE data
write.csv(FSSC_clay,"FSSC_clay.csv",row.names=F)
write.csv(FSSC_silt,"FSSC_silt.csv",row.names=F)
write.csv(FSSC_sand,"FSSC_sand.csv",row.names=F)


#for loops to calculate the average thickness weighted organic carbon content in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,23)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	averageOC=a[,2]*a[,3]
	I_OC=sum(averageOC)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,23)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	averageOC=b[,2]*b[,3]
	E_OC=sum(averageOC)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_OC
	temp2[i,3]<-I_OC

}

colnames(temp2)<-c("ID","E_OC","I_OC")
View(temp2)

FSSC_OC=temp2

#SAVE data
write.csv(FSSC_OC,"FSSC_OC.csv",row.names=F)


#for loops to calculate the average thickness weighted Porosity content in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,22)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	averageF=a[,2]*a[,3]
	I_F=sum(averageF)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,22)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	averageF=b[,2]*b[,3]
	E_F=sum(averageF)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_F
	temp2[i,3]<-I_F

}

colnames(temp2)<-c("ID","E_F","I_F")
View(temp2)

FSSC_Porosity=temp2

#SAVE data
write.csv(FSSC_Porosity,"FSSC_Porosity.csv",row.names=F)


#for loops to calculate the average thickness weighted pHw in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,24)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	averagepHw=a[,2]*a[,3]
	I_pHw=sum(averagepHw)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,24)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	averagepHw=b[,2]*b[,3]
	E_pHw=sum(averagepHw)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_pHw
	temp2[i,3]<-I_pHw

}

colnames(temp2)<-c("ID","E_pHw","I_pHw")
View(temp2)

FSSC_pHw=temp2

#SAVE data
write.csv(FSSC_pHw,"FSSC_pHw.csv",row.names=F)


#for loops to calculate the average thickness weighted pHKCL in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,25)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	averagepHkcl=a[,2]*a[,3]
	I_pHkcl=sum(averagepHkcl)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,25)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	averagepHkcl=b[,2]*b[,3]
	E_pHkcl=sum(averagepHkcl)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_pHkcl
	temp2[i,3]<-I_pHkcl

}

colnames(temp2)<-c("ID","E_pHkcl","I_pHkcl")
View(temp2)

FSSC_pHkcl=temp2

#SAVE data
write.csv(FSSC_pHkcl,"FSSC_pHkcl.csv",row.names=F)


#for loops to calculate the average thickness weighted total exchangeable bases in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,30)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values
	a[,3]<- as.numeric(as.character(a[,3]))	

	averageTEB=a[,2]*a[,3]
	I_TEB=sum(averageTEB)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,30)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values
	b[,3]<- as.numeric(as.character(b[,3]))

	averageTEB=b[,2]*b[,3]
	E_TEB=sum(averageTEB)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_TEB
	temp2[i,3]<-I_TEB

}

colnames(temp2)<-c("ID","E_TEB","I_TEB")
View(temp2)

FSSC_TEB=temp2

#SAVE data
write.csv(FSSC_TEB,"FSSC_TEB.csv",row.names=F)



#for loops to calculate the average thickness weighted CEC in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,31)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values
	a[,3]<- as.numeric(as.character(a[,3]))	

	averageCEC=a[,2]*a[,3]
	I_CEC=sum(averageCEC)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,31)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values
	b[,3]<- as.numeric(as.character(b[,3]))	

	averageCEC=b[,2]*b[,3]
	E_CEC=sum(averageCEC)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_CEC
	temp2[i,3]<-I_CEC

}

colnames(temp2)<-c("ID","E_CEC","I_CEC")
View(temp2)

FSSC_CEC=temp2

#SAVE data
write.csv(FSSC_CEC,"FSSC_CEC.csv",row.names=F)




#for loops to calculate the average thickness weighted base saturation in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,32)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values
	a[,3]<- as.numeric(as.character(a[,3]))

	averagebasesat=a[,2]*a[,3]
	I_BaseSat=sum(averagebasesat)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,32)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values
	b[,3]<- as.numeric(as.character(b[,3]))

	averagebasesat=b[,2]*b[,3]
	E_BaseSat=sum(averagebasesat)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_BaseSat
	temp2[i,3]<-I_BaseSat

}

colnames(temp2)<-c("ID","E_BaseSat","I_BaseSat")
View(temp2)

FSSC_BaseSat=temp2

#SAVE data
write.csv(FSSC_BaseSat,"FSSC_BaseSat.csv",row.names=F)



#for loops to calculate the average mineralogy in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,33)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	mean(a[,2], na.rm= TRUE)
	I_SM=mean

	b=subset(temp,temp$ZONE=="loss")[,c(2,33)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	mean(a, na.rm=TRUE)
	E_SM=mean

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_SM
	temp2[i,3]<-I_SM

}

colnames(temp2)<-c("ID","E_SM","I_SM")
View(temp2)

#for loops to calculate the average thickness weighted extractable acidity in zones of loss and accumulation 
holder=data.frame()
profiles=(unique(Horizons$ID))
temp2<-data.frame()
for(i in 1:length(profiles)){ 

	temp=subset(Horizons, Horizons$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]					#order profile by depth

	a=subset(temp,temp$ZONE=="gain")[,c(2,6,33)]			#Selecting horizons from each profile identified as being in the zone of accumulation	
	a[,2]<- as.numeric(as.character(a[,2]))				#converting factors to numeric values

	averageEXTRACIDITY=a[,2]*a[,3]
	I_EXTRACIDITY=sum(averageEXTRACIDITY)

	b=subset(temp,temp$ZONE=="loss")[,c(2,6,12)]			#Selecting horizons from each profile identified as being in the zone of loss	
	b[,2]<- as.numeric(as.character(b[,2]))				#converting factors to numeric values

	averageEXTRACIDITY=b[,2]*b[,3]
	E_EXTRACIDITY=sum(averageEXTRACIDITY)

	dull<-as.character(temp[1,2])
	temp2[i,1]<-dull
	temp2[i,2]<-E_EXTRACIDITY
	temp2[i,3]<-I_EXTRACIDITY

}

colnames(temp2)<-c("ID","E_EXTRACIDITY","I_EXTRACIDITY")
View(temp2)

FSSC_EXTRACIDITY=temp2
View(FSSC_EXTRACIDITY)


#SAVE data
write.csv(FSSC_EXTRACIDITY,"FSSC_Extracidity.csv",row.names=F)

