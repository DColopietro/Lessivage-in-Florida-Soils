#set your working directory
setwd("")


#read in data for the the shallowest positive argillic in all FSSC profiles
positives=data.frame(read.csv("FSSC_Lessivage Positive Profiles.csv",header=TRUE))
str(positives)


#read in texture and depth QAQC'd FSSC data
FSSC=data.frame(read.csv("FSSC_QAQCd for texture and depth.csv",header=TRUE))
str(FSSC)


#seperate positives into those from criteria one and criteria two
criteria.one=subset(positives,positives$CRITERIA=="one")
criteria.two=subset(positives,positives$CRITERIA=="two")


#make some useful lists
criteria.one.horizons=criteria.one$HorzIndex
criteria.one.profiles=criteria.one$ID

criteria.two.horizons=criteria.two$HorzIndex
criteria.two.profiles=criteria.two$ID


#subset QAQC'd data for profiles with a positive lessivage test according to criteria one
criteria.one.data=c()
for(i in 1:length(criteria.one.profiles)){
	temp=subset(FSSC[,1:7],FSSC$ID==paste(criteria.one.profiles[i]))
	criteria.one.data=rbind(criteria.one.data,temp)
}


#subset QAQC'd data for profiles with a positive lessivage test according to criteria two
criteria.two.data=c()
for(i in 1:length(criteria.two.profiles)){
	temp=subset(FSSC[,1:7],FSSC$ID==paste(criteria.two.profiles[i]))
	criteria.two.data=rbind(criteria.two.data,temp)
}


#confirm that 721 profiles and 35 profiles are accounted for by criteria one and two respectively
str(criteria.one.data)
length(unique(criteria.one.data$ID))
str(criteria.two.data)
length(unique(criteria.two.data$ID))


#for criteria one and criteria two data, create a new column (filled with "other") to identify the "zone"
criteria.one.data$ZONE="other"					 									
criteria.two.data$ZONE="other"					 									


#for criteria one and criteria two data, run forloops to replace "other" with "top" at the top clay accumulation
#be patient, it's slow, criteria one data are large
for(i in 1:nrow(criteria.one.data)){
	for(j in 1:length(criteria.one.horizons)){	
		if(criteria.one.data[i,1]==criteria.one.horizons[j]){criteria.one.data[i,8]="top"}
	}
}

for(i in 1:nrow(criteria.two.data)){
	for(j in 1:length(criteria.two.horizons)){	
		if(criteria.two.data[i,1]==criteria.two.horizons[j]){criteria.two.data[i,8]="top"}
	}
}


#confirm the product of these forloops
criteria.one.data[1:20,]
criteria.two.data[1:20,]


#for loops to identify the zone of loss (anything above "top") in each criteria one profile
holder=data.frame()
profiles=(unique(criteria.one.data$ID))
for(i in 1:length(profiles)){ 
	temp=subset(criteria.one.data,criteria.one.data$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]							#order profile by depth

	target=subset(temp,temp$ZONE=="top")$TOP_DEP					#identify the depth of "top"
	
	for(j in 1:nrow(temp)){									#identify any horizon above top as "loss" 
		if(temp[j,3]<target){temp[j,8]="loss"}
	}
	
	holder=rbind(holder,temp)								#hold and compile modified records here
}
criteria.one.data=holder


#for loops to identify the zone of loss (anything above "top") in each criteria two profile
holder=data.frame()
profiles=(unique(criteria.two.data$ID))
for(i in 1:length(profiles)){ 
	temp=subset(criteria.two.data,criteria.two.data$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]							#order profile by depth

	target=subset(temp,temp$ZONE=="top")$TOP_DEP					#identify the depth of "top"
	
	for(j in 1:nrow(temp)){									#identify any horizon above top as "loss" 
		if(temp[j,3]<target){temp[j,8]="loss"}
	}
	
	holder=rbind(holder,temp)								#hold and compile modified records here
}
criteria.two.data=holder


#confirm the product of these forloops
criteria.one.data[1:20,]
criteria.two.data[1:20,]



#for loops to identify the zone of accumulation (horizons below "top") in each criteria one profile
holder=data.frame()
profiles=(unique(criteria.one.data$ID))
for(i in 1:length(profiles)){ 
	temp=subset(criteria.one.data,criteria.one.data$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]							#order profile by depth

	a=subset(temp,temp$ZONE=="loss")[,c(3,4,7)]					#find "top" then set target clay for criteria one
	thick=a[,2]-a[,1]										#by calculating the thickness weighted average 
	weightedthick=thick/sum(thick)							#of clay conc. in zone of loss + 3%
	target=sum(weightedthick*a$TOT_CLAY)+3

	for(k in 1:nrow(temp)){									#compare clay in horizons below "top" to the target clay
		if(temp[k,8]=="other" & temp[k,7]>=target){temp[k,8]="gain"}
	}
	
	holder=rbind(holder,temp)								#hold and compile modified records here
}
criteria.one.data=holder

a=subset(temp,temp$ZONE=="loss")[,c(3,4,7)]
thick=a[,2]-a[,1]
weightedthick=thick/sum(thick)

sum(weightedthick*a$TOT_CLAY)


sum(weightedthick)

subset(temp,temp$ZONE==)



#for loops to identify the zone of accumulation (horizons below "top") in each criteria two profile
holder=data.frame()
profiles=(unique(criteria.two.data$ID))
for(i in 1:length(profiles)){ 
	temp=subset(criteria.two.data,criteria.two.data$ID==profiles[i]) 		#select one profile at a time
	temp=temp[order(temp$TOP_DEP),]							#order profile by depth

	a=subset(temp,temp$ZONE=="loss")[,c(3,4,7)]					#find "top" then set target clay for criteria one
	thick=a[,2]-a[,1]										#by calculating the thickness weighted average 
	weightedthick=thick/sum(thick)							#of clay conc. in zone of loss x 1.2
	target=sum(weightedthick*a$TOT_CLAY)*1.2
	
	for(k in 1:nrow(temp)){									#compare clay in horizons below "top" to the target clay
		if(temp[k,8]=="other" & temp[k,7]>=target){temp[k,8]="gain"}
	}
	
	holder=rbind(holder,temp)								#hold and compile modified records here
}
criteria.two.data=holder


#confirm the product of these forloops
criteria.one.data[1:20,]
criteria.two.data[1:20,]


#add column to identify criteria 
criteria.one.data$CRITERIA="one"
criteria.two.data$CRITERIA="two"


#combine criteria one and criteria two data
combined=rbind(criteria.one.data,criteria.two.data)


#confirm that 756 (721 + 35) profiles are accounted for in combined
str(combined)
length(unique(combined$ID))


#SAVE combined data
write.csv(combined,"FSSC_Lessivage Positive Profiles with Loss and Acumulation.csv",row.names=F)






