#read in raw FSSC data
data=data.frame(read.csv("FSSC_Raw Data.csv",header=TRUE))

#see that you have 8272 horizons from 1288 profiles 
str(data)
length(unique(data$ID))

#isolate the data of onterest (dataoi)
#right now that is columns 1,3,5,32,33,39,40, and 41
View(colnames(data))
dataoi=data[,c(1,3,5,32,33,39:41)]

#keep only records with trustworthy texture data by checking sand+silt+clay >98 and <102
dataoi$sum=data$TOT_SAND+data$TOT_SILT+data$TOT_CLAY
temp=subset(dataoi,dataoi$sum<102)
dataoi=subset(temp,temp$sum>98)

#see that you have 7541 horizons from 1226 profiles 
str(dataoi)
length(unique(dataoi$ID))

#keep profiles with more than 1 horizon
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

#keep only records with trustworthy depth data by checking tops and bottoms coincide
holder=data.frame()
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
	
	temp=subset(dataoi,dataoi$ID==profiles[i])			#select one profile at a time	
	temp=temp[order(temp$HorzIndex),]					#order the profile by depth
	
	comparisons=c()								
	for(j in 2:nrow(temp)){							#starting at 2nd horizon, evaluate each horizon seperately
		difference=temp[j,3]==temp[j-1,4]				#compare each top to overlying bottom
		comparisons=c(comparisons,difference)			#compile comparisons for the profile
	}

	if(sum(comparisons)==j-1){holder=rbind(holder,temp)}		#compile profiles with coincident tops and bottoms 	

}
dataoi=holder

#see that you have 7130 horizons from 1140 profiles 
str(dataoi)
length(unique(dataoi$ID))

#keep profiles with tops 20cm or less
holder=data.frame()
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){ 
	temp=subset(dataoi,dataoi$ID==profiles[i]) 
	if(min(temp$TOP_DEP)<20){holder=rbind(holder,temp)}
}
dataoi=holder

#see that you have 6965 horizons from 1100 profiles 
str(dataoi)
length(unique(dataoi$ID))

#before evaluating for lessivage, save dataoi because it is a QAQC'd texture and depth dataset
write.csv(dataoi,"FSSC_QAQCd for texture and depth.csv",row.names=F)

#evaluate data of interest for criteria one
criteria.one.horizons=data.frame() 
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
	
	temp=subset(dataoi,dataoi$ID==profiles[i])									#select one profile at a time	
	temp=temp[order(temp$HorzIndex),]											#order the profile by depth
	
	for(j in 2:nrow(temp)){													#starting at 2nd horizon, evaluate each horizon seperately
		temp2=rbind(
			subset(temp,temp[j,3]-temp$BOTT_DEP<30 & temp[j,3]-temp$BOTT_DEP>=0),			#combine overlying horizons within 30cm
		temp[j,])														#with the horizon of interest 		
	
		for (k in 1:(nrow(temp2)-1)){ 
			if(temp2$TOT_CLAY[k]<15 & temp2$TOT_CLAY[nrow(temp2)]-temp2$TOT_CLAY[k]>=3){ 		#evaluate each 30cm bundle for criteria 1
				criteria.one.horizons=rbind(criteria.one.horizons,temp2[nrow(temp2),c(1:3)])} #record HorzIndex, ID, and TOP_DEP of each positive case
}}}

#see that you have 721 profiles with a positive test for criteria one
length(unique(criteria.one.horizons$ID))

#Remove profiles positive for criteria one from the data of interest 
criteria.one.profiles=unique(criteria.one.horizons$ID) 
dataoi=dataoi[!dataoi$ID %in% criteria.one.profiles,] 

#see that you have 2058 horizons from 379 profiles left to evaluate for criteria two and three
str(dataoi)
length(unique(dataoi$ID))

#evaluate data of interest for criteria two
criteria.two.horizons=data.frame() 
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
	
	temp=subset(dataoi,dataoi$ID==profiles[i])									#select one profile at a time	
	temp=temp[order(temp$HorzIndex),]											#order the profile by depth
	
	for(j in 2:nrow(temp)){													#starting at 2nd horizon, evaluate each horizon seperately
		temp2=rbind(
			subset(temp,temp[j,3]-temp$BOTT_DEP<30 & temp[j,3]-temp$BOTT_DEP>=0),			#combine overlying horizons within 30cm
		temp[j,])														#with the horizon of interest 		
	
		for (k in 1:(nrow(temp2)-1)){ 
			if(temp2$TOT_CLAY[k]>=15 & temp2$TOT_CLAY[k]<=40 & 						#evaluate each 30cm bundle for criteria 2
			temp2$TOT_CLAY[nrow(temp2)]>=temp2$TOT_CLAY[k]*1.2){
			criteria.two.horizons=rbind(criteria.two.horizons,temp2[nrow(temp2),c(1:3)])} 	#record HorzIndex, ID, and TOP_DEP of each positive case
}}}

#see that you have 35 profiles with a positive test for criteria two
length(unique(criteria.two.horizons$ID))

#Remove profiles positive for criteria two from the data of interest 
criteria.two.profiles=unique(criteria.two.horizons$ID) 
dataoi=dataoi[!dataoi$ID %in% criteria.two.profiles,] 

#see that you have 1877 horizons from 344 profiles left to evaluate for criteria two and three
str(dataoi)
length(unique(dataoi$ID))

#evaluate data of interest for criteria three
criteria.three.horizons=data.frame() 
profiles=(unique(dataoi$ID))
for(i in 1:length(profiles)){
	
	temp=subset(dataoi,dataoi$ID==profiles[i])									#select one profile at a time	
	temp=temp[order(temp$HorzIndex),]											#order the profile by depth
	
	for(j in 2:nrow(temp)){													#starting at 2nd horizon, evaluate each horizon seperately
		temp2=rbind(
			subset(temp,temp[j,3]-temp$BOTT_DEP<30 & temp[j,3]-temp$BOTT_DEP>=0),			#combine overlying horizons within 30cm
		temp[j,])														#with the horizon of interest 		
	
		for (k in 1:(nrow(temp2)-1)){ 
			if(temp2$TOT_CLAY[k]>40 & temp2$TOT_CLAY[nrow(temp2)]-temp2$TOT_CLAY[k]>=8){		#evaluate each 30cm bundle for criteria 3
			criteria.three.horizons=rbind(criteria.three.horizons,temp2[nrow(temp2),c(1:3)])}	#record HorzIndex, ID, and TOP_DEP of each positive case
}}}

#see that you have 0 profiles with a positive test for criteria three
length(unique(criteria.three.horizons$ID))

#look at lists of horizons that test positive for crietria one and two
View(criteria.one.horizons)
View(criteria.two.horizons)

#Since there will be duplicate horizon records (HorzIndex), remove the duplicate horizon records 
criteria.one.horizons=criteria.one.horizons[!duplicated(criteria.one.horizons[ , c("HorzIndex")]),]
criteria.two.horizons=criteria.two.horizons[!duplicated(criteria.two.horizons[ , c("HorzIndex")]),]

#confirm you indeed have 721 and 35 profiles for criteria one and two respectively
length(unique(criteria.one.horizons$ID))
length(unique(criteria.two.horizons$ID))

#add column to identify 
criteria.one.horizons$CRITERIA="one"
criteria.two.horizons$CRITERIA="two"
str(criteria.one.horizons)
str(criteria.two.horizons)

#retain only the shallowest positive test for lessivage in each profile 
#(by sorting and then removing duplicates, the shallowest horizins will be retained)
criteria.one.horizons=criteria.one.horizons[order(criteria.one.horizons[,2],criteria.one.horizons[,3]),]
criteria.two.horizons=criteria.two.horizons[order(criteria.two.horizons[,2],criteria.two.horizons[,3]),]

criteria.one.horizons=criteria.one.horizons[!duplicated(criteria.one.horizons[ , c("ID")]),]
criteria.two.horizons=criteria.two.horizons[!duplicated(criteria.two.horizons[ , c("ID")]),]

#confirm you indeed have 721 and 35 horizons from 721 and 35 profiles for criteria one and two respectively
str(criteria.one.horizons)
length(unique(criteria.one.horizons$ID))
str(criteria.two.horizons)
length(unique(criteria.two.horizons$ID))

#combine positives for criteria one and two into a single dataframe and save it
write.csv(rbind(criteria.one.horizons,criteria.two.horizons),"FSSC_Lessivage Positive Profiles.csv",row.names=F)
