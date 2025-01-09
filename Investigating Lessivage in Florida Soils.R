#Read in Data####
setwd("")

data=data.frame(read.csv("Raw Data.csv",header=TRUE))

#see that you have 8272 horizons from 1288 profiles 
str(data)
length(unique(data$ID))

#isolate the data of interest (dataoi)
#right now that is columns 1,3,26,30-40
View(colnames(data))
dataoi=data[,c(1,3,26,30:40)]

#QAQC Data####################
##Check sand+silt+clay >98 and <102####
dataoi$sum=data$TOT_SAND+data$TOT_SILT+data$TOT_CLAY
temp=subset(dataoi,dataoi$sum<102)
dataoi=subset(temp,temp$sum>98)

#see that you have 7542 horizons from 1226 profiles
str(dataoi)
length(unique(dataoi$ID))

##Profiles with >1 horizon####
holder=data.frame()
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
	temp=subset(dataoi,dataoi$ID==profiles[i])
	if(nrow(temp)>1){holder=rbind(holder,temp)}
}
dataoi=holder

#see that you have 7519 horizons from 1204 profiles 
str(dataoi)
length(unique(dataoi$ID))

##Organize profiles by depth####
holder=data.frame()
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
  
  temp=subset(dataoi,dataoi$ID==profiles[i])			#select one profile at a time	
  temp=temp[order(temp$HorzIndex),]					#order the profile by depth
  holder=rbind(holder,temp)           #compile profiles
  
}
dataoi=holder

##Top horizon <20cm from the surface####
holder=data.frame()
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){ 
	temp=subset(dataoi,dataoi$ID==profiles[i]) 
	if(min(temp$TOP_DEP)<20){holder=rbind(holder,temp)}
}
dataoi=holder

#see that you have 7360 horizons from 1164 profiles 
str(dataoi)
length(unique(dataoi$ID))

##SAVE QAQC'd Data####
write.csv(dataoi,"QAQCd for texture and depth.csv",row.names=F) 

#Lessivage Evaluation####################
##Evaluate Data by Argillic Criteria 2A (one)####
criteria.one.horizons=data.frame() 
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
	
	temp=subset(dataoi,dataoi$ID==profiles[i])									#select one profile at a time	
	temp=temp[order(temp$HorzIndex),]											#order the profile by depth
	
	for(j in 2:nrow(temp)){								#starting at 2nd horizon, evaluate each horizon separately
		temp2=rbind(
			subset(temp,temp[j,4]-temp$BOTT_DEP<30 & temp[j,4]-temp$BOTT_DEP>=0),			#combine overlying horizons within 30cm
		temp[j,])														#with the horizon of interest 		
	
		for (k in 1:(nrow(temp2))){ 
			if(temp2$TOT_CLAY[k]<15 & temp2$TOT_CLAY[nrow(temp2)]-temp2$TOT_CLAY[k]>=3){ 		#evaluate each 30cm bundle for criteria 1
				criteria.one.horizons=rbind(criteria.one.horizons,temp2[nrow(temp2),c(1:4)])} #record HorzIndex, ID, HNAME, and TOP_DEP of each positive case
}}}

#see that you have 721 (763) profiles with a positive test for criteria one
length(unique(criteria.one.horizons$ID))

#Remove profiles positive for criteria one from the data of interest 
criteria.one.profiles=unique(criteria.one.horizons$ID) 
dataoi=dataoi[!dataoi$ID %in% criteria.one.profiles,] 

#see that you have 2058 (2171) horizons from 379 (401) profiles left to evaluate for criteria two and three
str(dataoi)
length(unique(dataoi$ID))

##Evaluate Data by Argillic Criteria 2B (two)####
criteria.two.horizons=data.frame() 
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
	
	temp=subset(dataoi,dataoi$ID==profiles[i])									#select one profile at a time	
	temp=temp[order(temp$HorzIndex),]											#order the profile by depth
	
	for(j in 2:nrow(temp)){													#starting at 2nd horizon, evaluate each horizon seperately
		temp2=rbind(
			subset(temp,temp[j,4]-temp$BOTT_DEP<30 & temp[j,4]-temp$BOTT_DEP>=0),			#combine overlying horizons within 30cm
		temp[j,])														#with the horizon of interest 		
	
		for (k in 1:(nrow(temp2))){ 
			if(temp2$TOT_CLAY[k]>=15 & temp2$TOT_CLAY[k]<=40 & 						#evaluate each 30cm bundle for criteria 2
			temp2$TOT_CLAY[nrow(temp2)]>=temp2$TOT_CLAY[k]*1.2){
			criteria.two.horizons=rbind(criteria.two.horizons,temp2[nrow(temp2),c(1:4)])} 	#record HorzIndex, ID, HNAME, and TOP_DEP of each positive case
}}}

#see that you have 35 (38) profiles with a positive test for criteria two
length(unique(criteria.two.horizons$ID))

#Remove profiles positive for criteria two from the data of interest 
criteria.two.profiles=unique(criteria.two.horizons$ID) 
dataoi=dataoi[!dataoi$ID %in% criteria.two.profiles,] 

#see that you have 1877 (1974) horizons from 344 (363) profiles left to evaluate for criteria two and three
str(dataoi)
length(unique(dataoi$ID))

##Evaluate Data by Argillic Criteria 2C (three)####
criteria.three.horizons=data.frame() 
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
	
	temp=subset(dataoi,dataoi$ID==profiles[i])									#select one profile at a time	
	temp=temp[order(temp$HorzIndex),]											#order the profile by depth
	
	for(j in 2:nrow(temp)){													#starting at 2nd horizon, evaluate each horizon seperately
		temp2=rbind(
			subset(temp,temp[j,4]-temp$BOTT_DEP<30 & temp[j,4]-temp$BOTT_DEP>=0),			#combine overlying horizons within 30cm
		temp[j,])														#with the horizon of interest 		
	
		for (k in 1:(nrow(temp2))){ 
			if(temp2$TOT_CLAY[k]>40 & temp2$TOT_CLAY[nrow(temp2)]-temp2$TOT_CLAY[k]>=8){		#evaluate each 30cm bundle for criteria 3
			criteria.three.horizons=rbind(criteria.three.horizons,temp2[nrow(temp2),c(1:4)])}	#record HorzIndex, ID, HNAME, and TOP_DEP of each positive case
}}}

#see that you have 0 profiles with a positive test for criteria three
length(unique(criteria.three.horizons$ID))

#look at lists of horizons that test positive for crietria one and two
View(criteria.one.horizons)
View(criteria.two.horizons)

#Since there will be duplicate horizon records (HorzIndex), remove the duplicate horizon records 
criteria.one.horizons=criteria.one.horizons[!duplicated(criteria.one.horizons[ , c("HorzIndex")]),]
criteria.two.horizons=criteria.two.horizons[!duplicated(criteria.two.horizons[ , c("HorzIndex")]),]

#confirm you indeed have 721 (763) and 35 (38) profiles for criteria one and two respectively
length(unique(criteria.one.horizons$ID))
length(unique(criteria.two.horizons$ID))

#add column to identify 
criteria.one.horizons$CRITERIA="one"
criteria.two.horizons$CRITERIA="two"
str(criteria.one.horizons)
str(criteria.two.horizons)

#retain only the shallowest positive test for lessivage in each profile 
#(by sorting and then removing duplicates, the shallowest horizons will be retained)
criteria.one.horizons=criteria.one.horizons[order(criteria.one.horizons[,2],criteria.one.horizons[,3]),]
criteria.two.horizons=criteria.two.horizons[order(criteria.two.horizons[,2],criteria.two.horizons[,3]),]

criteria.one.horizons=criteria.one.horizons[!duplicated(criteria.one.horizons[ , c("ID")]),]
criteria.two.horizons=criteria.two.horizons[!duplicated(criteria.two.horizons[ , c("ID")]),]

#confirm you indeed have 721 (763) and 35 (38) horizons from 721 and 35 profiles for criteria one and two respectively
str(criteria.one.horizons)
length(unique(criteria.one.horizons$ID))
str(criteria.two.horizons)
length(unique(criteria.two.horizons$ID))

#combine positives for criteria one and two into a single dataframe and save it
write.csv(rbind(criteria.one.horizons,criteria.two.horizons),"Lessivage Positive Profiles.csv",row.names=F)

#read in data for the the shallowest positive argillic in all profiles
positives=data.frame(read.csv("Lessivage Positive Profiles.csv",header=TRUE))
str(positives)

#read in texture and depth QAQC'd data
QAQC=data.frame(read.csv("QAQCd for texture and depth.csv",header=TRUE))
str(QAQC)

#separate positives into those from criteria one and criteria two
criteria.one=subset(positives,positives$CRITERIA=="one")
criteria.two=subset(positives,positives$CRITERIA=="two")

#Subset by Argillic Criteria####
#make some useful lists
criteria.one.horizons=criteria.one$HorzIndex
criteria.one.profiles=criteria.one$ID

criteria.two.horizons=criteria.two$HorzIndex
criteria.two.profiles=criteria.two$ID

#subset QAQC'd data for profiles meeting the requirements of criteria 2A (one)
criteria.one.data=c()
for(i in 1:length(criteria.one.profiles)){
  temp=subset(QAQC[,1:14],QAQC$ID==paste(criteria.one.profiles[i]))
  criteria.one.data=rbind(criteria.one.data,temp)
}

#subset QAQC'd data for profiles meeting the requirements of criteria 2B (two)
criteria.two.data=c()
for(i in 1:length(criteria.two.profiles)){
  temp=subset(QAQC[,1:14],QAQC$ID==paste(criteria.two.profiles[i]))
  criteria.two.data=rbind(criteria.two.data,temp)
}

#confirm that 721 (763) profiles and 35 (38) profiles are accounted for by criteria one and two respectively
str(criteria.one.data)
length(unique(criteria.one.data$ID))
str(criteria.two.data)
length(unique(criteria.two.data$ID))

#for criteria one and criteria two data, create a new column (filled with "other") to identify the "zone"
criteria.one.data$ZONE="other"					 									
criteria.two.data$ZONE="other"					 									

#Identify Zones of Loss and Accumulation####################
##Identify the top of the zone of accumulation####
#for criteria one and criteria two data, run forloops to replace "other" with "top" at the top clay accumulation
for(i in 1:nrow(criteria.one.data)){
  for(j in 1:length(criteria.one.horizons)){	
    if(criteria.one.data[i,1]==criteria.one.horizons[j]){criteria.one.data[i,15]="top"}
  }
}

for(i in 1:nrow(criteria.two.data)){
  for(j in 1:length(criteria.two.horizons)){	
    if(criteria.two.data[i,1]==criteria.two.horizons[j]){criteria.two.data[i,15]="top"}
  }
}

#confirm the product of these forloops
criteria.one.data[1:15,]
criteria.two.data[1:15,]

##Identify the zone of loss (horizons above "top") in each criteria one profile####
holder=data.frame()
profiles=(unique(criteria.one.data$ID))
for(i in 1:length(profiles)){ 
  temp=subset(criteria.one.data,criteria.one.data$ID==profiles[i]) 		#select one profile at a time
  temp=temp[order(temp$TOP_DEP),]							#order profile by depth
  
  target=subset(temp,temp$ZONE=="top")$TOP_DEP					#identify the depth of "top"
  
  for(j in 1:nrow(temp)){									#identify any horizon above top as "loss" 
    if(temp[j,4]<target){temp[j,15]="loss"}
  }
  
  holder=rbind(holder,temp)								#hold and compile modified records here
}
criteria.one.data=holder

##Identify the zone of loss (horizons above "top") in each criteria two profile####
holder=data.frame()
profiles=(unique(criteria.two.data$ID))
for(i in 1:length(profiles)){ 
  temp=subset(criteria.two.data,criteria.two.data$ID==profiles[i]) 		#select one profile at a time
  temp=temp[order(temp$TOP_DEP),]							#order profile by depth
  
  target=subset(temp,temp$ZONE=="top")$TOP_DEP					#identify the depth of "top"
  
  for(j in 1:nrow(temp)){									#identify any horizon above top as "loss" 
    if(temp[j,4]<target){temp[j,15]="loss"}
  }
  
  holder=rbind(holder,temp)								#hold and compile modified records here
}
criteria.two.data=holder

#confirm the product of these forloops
criteria.one.data[1:15,]
criteria.two.data[1:15,]

##Identify the zone of accumulation (horizons below "top") in each criteria one profile####
holder=data.frame()
profiles=(unique(criteria.one.data$ID))
for(i in 1:length(profiles)){ 
  temp=subset(criteria.one.data,criteria.one.data$ID==profiles[i]) 		#select one profile at a time
  temp=temp[order(temp$TOP_DEP),]							#order profile by depth
  
  a=subset(temp,temp$ZONE=="top")[,c(4,5,13)]					#find "top" then set target clay for criteria one
  thick=a[,2]-a[,1]										#by calculating the thickness weighted average 
  weightedthick=thick/sum(thick)							#of clay conc. in zone of loss + 3%
  target=sum(weightedthick*a$TOT_CLAY)+3
  
  for(k in 1:nrow(temp)){									#compare clay in horizons below "top" to the target clay
    if(temp[k,15]=="other" & temp[k,13]>=target){temp[k,15]="gain"}
  }
  
  holder=rbind(holder,temp)								#hold and compile modified records here
}
criteria.one.data=holder

##Identify the zone of accumulation (horizons below "top") in each criteria two profile####
holder=data.frame()
profiles=(unique(criteria.two.data$ID))
for(i in 1:length(profiles)){ 
  temp=subset(criteria.two.data,criteria.two.data$ID==profiles[i]) 		#select one profile at a time
  temp=temp[order(temp$TOP_DEP),]							#order profile by depth
  
  a=subset(temp,temp$ZONE=="top")[,c(4,5,13)]					#find "top" then set target clay for criteria one
  thick=a[,2]-a[,1]										#by calculating the thickness weighted average 
  weightedthick=thick/sum(thick)							#of clay conc. in zone of loss x 1.2
  target=sum(weightedthick*a$TOT_CLAY)*1.2
  
  for(k in 1:nrow(temp)){									#compare clay in horizons below "top" to the target clay
    if(temp[k,15]=="other" & temp[k,13]>=target){temp[k,15]="gain"}
  }
  
  holder=rbind(holder,temp)								#hold and compile modified records here
}
criteria.two.data=holder

#confirm the product of these forloops
criteria.one.data[1:15,]
criteria.two.data[1:15,]

#add column to identify criteria 
criteria.one.data$CRITERIA="one"
criteria.two.data$CRITERIA="two"

#combine criteria one and criteria two data
combined=rbind(criteria.one.data,criteria.two.data)

#confirm that 756 (721 + 35) 801 (763 + 38) profiles are accounted for in combined
str(combined)
length(unique(combined$ID))

###SAVE Data Profiles with Identified Zones Data####
write.csv(combined,"Lessivage Positive Profiles with Loss and Acumulation.csv",row.names=F)

#read in profiles with identified zones
data=data.frame(read.csv("Lessivage Positive Profiles with Loss and Acumulation.csv",header=TRUE))
str(data)
View(colnames(data))

#Evaluating the UI and Sand Factions####################
#see that you have 5088 horizons from 756 (801) profiles 
str(data)
length(unique(data$ID))

#notice that sand fractions have some missing data, replace them with zeros
summary(data)

#compile profiles with missing sand fraction data
incompletesand=c()
for(i in 1:nrow(data)){
  if(is.na(data[i,10])==TRUE){temp=subset(data,data$ID==data[i,2])}
  if(is.na(data[i,10])==TRUE){incompletesand=rbind(incompletesand,temp)}
}

#remove duplicate horizons from incompletesand and see you have 2 profiles with missing data
incompletesand=incompletesand[!duplicated(incompletesand[ , c("HorzIndex")]),]
length(unique(incompletesand$ID))
View(incompletesand)

incompletesand$check=round(incompletesand$TOT_SAND-(incompletesand$SAND_C+
                                                      incompletesand$SAND_M+incompletesand$SAND_F+incompletesand$SAND_VF),1)
View(incompletesand)

#now that you've dealt with missing data, it's time to QAQC the sand fraction data by calculating
#the absolute value of the difference between reported total sand the sum of sand fractions
data$check=round(abs(data$TOT_SAND-(data$SAND_VC+data$SAND_C+data$SAND_M+data$SAND_F+data$SAND_VF)),1)

#see that you have 23 horizons from 21 profiles where the sum of sand fractions is not within 2% of total sand  
plot(data$check,log="y")
str(subset(data,data$check>2))
length(unique(subset(data,data$check>2)$ID))

#create a list of profiles with missing or spurious sand fraction data
temp1=subset(data,data$check>2)
temp2=subset(data,is.na(data$SAND_VC)==TRUE)
temp3=rbind(temp1,temp2)
questionablesand=unique(temp3$ID)

#create new data frame and profiles by removing questionable sand fractions
dataforUI=data[!data$ID %in% questionablesand,] 

#see that you have 4939 (5230) horizons from 734 (778) profiles ready for uniformity index estimates 
str(dataforUI)
length(unique(dataforUI$ID))

##Calculating UI####
#calculate uniformity value for each horizon
dataforUI$UV=(dataforUI$TOT_SILT+dataforUI$SAND_VF)/(dataforUI$TOT_SAND-dataforUI$SAND_VF)
summary(dataforUI$UV)

#run forloop to calculate UI of each horizon
holder=data.frame()
profiles=(unique(dataforUI$ID))
for(i in 1:length(profiles)){ 
  temp=subset(dataforUI,dataforUI$ID==profiles[i]) 			#select one profile at a time
  temp=temp[order(temp$TOP_DEP),]						#order profile by depth
  
  for(j in 2:nrow(temp)){								#calculate uniformity index of each horizon 
    temp[j,19]=abs(temp[j,18]/temp[j-1,18]-1)
  }
  
  holder=rbind(holder,temp)							#hold and compile calculations
}
final=holder
colnames(final)[19]="UI"

#see that you have 4939 (5230) horizons from 734 (778) profiles 
str(final)
length(unique(final$ID))

#Replace NAs for UI (which come from the surface horizons) with zeros
for (i in 1:nrow(final)){
  if(is.na(final[i,19])==TRUE){final[i,19]=0}
}

#Create new column identifying horizons with UI>0.8
final$DISCONT="no"
for (i in 1:nrow(final)){
  if(final[i,19]>0.8){final[i,20]="yes"}
}

#cleaning the dataframe
final$UI=round(final$UI,3)			#get rid of useless decimal places
final=final[,c(1:5,15:16,19:20,6:13)]	#reorganize data.frame column and drop some things you don't need
str(final)
View(final)

###SAVE Final Database Data####
write.csv(final,"Final Lessivage Database.csv",row.names=F)

#read in data for the positive profile horizons after profiles with UI>0.8 have been removed 
Horizons=data.frame(read.csv("Horizon Database.csv",header=TRUE))
str(Horizons)

#Calculating thickness of zones####
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

###SAVE Data####
write.csv(Thickness,"Thickness of Loss and Accumulation.csv",row.names=F)

#Calculate Soil Properties as Thickness Weighted Average by Zone####################
##Calculating the relative contribution of each horizon to the zone of loss#### 
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

###SAVE weighted contribution loss Data####
write.csv(Weightedcontribution,"WeightedcontributionLoss.csv",row.names=F)

##Calculating the relative contribution of each horizon to the zone of accumulation####
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

###SAVE weighted contribution gain Data####
write.csv(Weightedcontribution,"WeightedcontributionGain.csv",row.names=F)


#read in data for the positive profile horizons after weighted contributions column was added 
Horizons=data.frame(read.csv("Horizon Database_1.csv",header=TRUE))
str(Horizons)


##Calculate the thickness weighted average clay content in zones of loss and accumulation####
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

IE_ratio=temp2
View(IE_ratio)

E_ratio$IE=0
View(IE_ratio)

##Calculating the IE ratio####
IE_ratio$IE=IE_ratio[,3]/IE_ratio[,2]
View(IE_ratio)

###SAVE IE Ratio Data####
write.csv(IE_ratio,"IE_Ratio.csv",row.names=F)


##Calculate the thickness weighted average sand content in zones of loss and accumulation#### 
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

sand=temp2
View(sand)


##Calculate the thickness weighted average silt content in zones of loss and accumulation#### 
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

silt=temp2
View(silt)

##Calculate the thickness weighted average clay content in zones of loss and accumulation#### 
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

clay=temp2
View(clay)

###SAVE Texture Data####
write.csv(clay,"clay.csv",row.names=F)
write.csv(silt,"silt.csv",row.names=F)
write.csv(sand,"sand.csv",row.names=F)

##Calculate the thickness weighted average organic carbon content in zones of loss and accumulation#### 
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

OC=temp2

###SAVE OC Data####
write.csv(OC,"OC.csv",row.names=F)


##Calculate the thickness weighted average Porosity in zones of loss and accumulation#### 
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

Porosity=temp2

###SAVE Porosity Data####
write.csv(Porosity,"Porosity.csv",row.names=F)

##Calculate the thickness weighted average pHw in zones of loss and accumulation#### 
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

pHw=temp2

###SAVE pHw Data####
write.csv(pHw,"pHw.csv",row.names=F)

##Calculate the thickness weighted average pHKCL in zones of loss and accumulation#### 
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

pHkcl=temp2

###SAVE pHKcl Data####
write.csv(pHkcl,"pHkcl.csv",row.names=F)

##Calculate the thickness weighted average TEB in zones of loss and accumulation####
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

TEB=temp2

###SAVE TEB Data####
write.csv(TEB,"TEB.csv",row.names=F)

##Calculate the thickness weighted average CEC in zones of loss and accumulation#### 
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

CEC=temp2

###SAVE CEC Data####
write.csv(CEC,"CEC.csv",row.names=F)

##Calculate the thickness weighted average BS in zones of loss and accumulation#### 
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

BaseSat=temp2

###SAVE BS Data####
write.csv(BaseSat,"BaseSat.csv",row.names=F)

##Calculate the thickness weighted average EA in zones of loss and accumulation#### 
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

EXTRACIDITY=temp2
View(EXTRACIDITY)

###SAVE EA Data####
write.csv(FSSC_EXTRACIDITY,"Extracidity.csv",row.names=F)
