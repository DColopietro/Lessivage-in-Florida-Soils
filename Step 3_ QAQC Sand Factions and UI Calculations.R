#set working directory
setwd("")


#read in database of lessivage positive profiles with zone of loss and accumulation
data=data.frame(read.csv("FSSC_Lessivage Positive Profiles with Loss and Acumulation.csv",header=TRUE))
str(data)


#read in entire FSSC database
fssc=data.frame(read.csv("FSSC_All Data.csv",header=TRUE))
str(fssc)
View(colnames(fssc))


#add sand size fractions data from FSSC to lessivage profiles
data=merge(data,fssc[,c(1,34:38)],by="HorzIndex")


#see that you have 5088 horizons from 756 profiles 
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


#remove duplicate horizons from incompletesand and see you have 5 profiles with missing data
incompletesand=incompletesand[!duplicated(incompletesand[ , c("HorzIndex")]),]
length(unique(incompletesand$ID))
View(incompletesand)


#for profiles S13_006 and S32_003 you cannot do much because all sand fractions are missing
#in the entire profile (S13_006) or in particular horizons (S32_003)


#because only SAND_VC is missing in some or all horizons of the other three profiles 
#(S23_019, S62_015, and S66_033) go ahead and compare the sum of reported sand fractions
#to total sand...you should notice that these particular NAs should indeed be zeros
#or in the case of HorzIndex=6201501 should be 0.3
incompletesand$check=round(incompletesand$TOT_SAND-(incompletesand$SAND_C+
	incompletesand$SAND_M+incompletesand$SAND_F+incompletesand$SAND_VF),1)
View(incompletesand)



#run forloop to poulate each missing SAND_VC in profiles S23_019, S62_015, and S66_033
#with the manually calculated value 
for(i in 1:nrow(data)){
	if(data[i,1]==2301906){data[i,10]=0.0}	#one if/then statement for each horizon
	if(data[i,1]==6201501){data[i,10]=0.3}
	if(data[i,1]==6603301){data[i,10]=0.0}
	if(data[i,1]==6603302){data[i,10]=0.0}
	if(data[i,1]==6603303){data[i,10]=0.0}
	if(data[i,1]==6603304){data[i,10]=0.0}
	if(data[i,1]==6603305){data[i,10]=0.0}
	if(data[i,1]==6603306){data[i,10]=0.0}
	if(data[i,1]==6603307){data[i,10]=0.0}
}


#see that have you now only have six horizons with missing sand fraction data (in profiles S13_006 and S32_003) 
summary(data)


#now that you've dealt with missing data, it's time to QAQC the sand fraction data by calculating
#the absolute value of the difference beteewn reported total sand the sum of sand fractions
data$check=round(abs(data$TOT_SAND-(data$SAND_VC+data$SAND_C+data$SAND_M+data$SAND_F+data$SAND_VF)),1)


#see that you have 22 horizons from 20 profiles where the sum of sand fractions is not within 2% of total sand  
plot(data$check,log="y")
str(subset(data,data$check>2))
length(unique(subset(data,data$check>2)$ID))


#create a list of profiles with missing or spurious sand fraction data
temp1=subset(data,data$check>2)
temp2=subset(data,is.na(data$SAND_VC)==TRUE)
temp3=rbind(temp1,temp2)
questionablesand=unique(temp3$ID)


#create new data frame sans profiles with questionable sand fractions
dataforUI=data[!data$ID %in% questionablesand,] 


#see that you have 4939 horizons from 734 profiles ready for uniformity index estimates 
str(dataforUI)
length(unique(dataforUI$ID))


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
		temp[j,17]=abs(temp[j,16]/temp[j-1,16]-1)
	}
	
	holder=rbind(holder,temp)							#hold and compile calculations
}
final=holder
colnames(final)[17]="UI"

#see that you STILL HAVE 4939 horizons from 734 profiles 
str(final)
length(unique(final$ID))


#Replace NAs for UI (which come from the surface horizons) with zeros
for (i in 1:nrow(final)){
	if(is.na(final[i,17])==TRUE){final[i,17]=0}
}


#Create new column identifying horizons with UI>0.8
final$DISCONT="no"
for (i in 1:nrow(final)){
	if(final[i,17]>0.8){final[i,18]="yes"}
}



#clean some things up
final$UI=round(final$UI,3)			#get rid of useless decimal places
final=final[,c(1:4,9,8,17,18,5:7,10:14)]	#reorganize data.frame column and drop some things you don't need
str(final)
View(final)


#save the final database
write.csv(final,"FSSC_Final Lessivage Database.csv",row.names=F)
