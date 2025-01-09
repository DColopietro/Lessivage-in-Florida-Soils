#Read in Data####################
setwd("")

data=data.frame(read.csv("Profile Database.csv",header=TRUE))
str(data)

data$Deposit.Material=as.factor(data$Deposit.Material)
data$Drainage.Class=as.factor(data$Drainage.Class)
data$E_DOMMIN=as.factor(data$E_DOMMIN)
data$I_DOMMIN=as.factor(data$I_DOMMIN)

View(colnames(data))

library(lattice)
library(randomForest)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(doParallel)

#Depth of lessivage####################
#Removed degree of lessivage, and taxonomic information
Zinputs=data[,c(4,11:50)]
str(Zinputs)

##Tune for depth of lessivage for ntree and mtry#Read in Data##################
ntree=seq(100,1000,25)
nvariables=seq(2,20,1)

tuningresults=data.frame()

for(i in 1:length(ntree)){
  for(j in 1:length(nvariables)){
    tune=c(ntree[i],nvariables[j])
    set.seed(520)
    mod=randomForest(Zlessivage~.,data=Zinputs,na.action=na.exclude,ntree=tune[1],mtry=tune[2])
    temp=c(tune[1],tune[2],mod$mse[tune[1]])
    tuningresults=rbind(tuningresults,temp)
  }}
colnames(tuningresults)=c("ntree","mtry","mse")
plot(tuningresults$mse~tuningresults$ntree)

#View optimal ntree and mtry for Z lessivage
subset(tuningresults,tuningresults$mse==min(tuningresults$mse))

##Random forest models for Zlessivage##################
#Run 50 random forest models for Zlessivage and store variable importances & R squareds
zimp=c()
oobRsqr=c()
Rsqr=c()
RSME=c()
set.seed(1)
for(i in 1:50){
  #Randomly create calibration (80%) and validation (20%) datasets
  train=Zinputs[sample(nrow(Zinputs),316), ]
  test=Zinputs
  temp=unique(row.names(train))
  for(j in 1:nrow(train)){test=subset(test,row.names(test)!=temp[j])}
  #run model 50 times with unique cal and val datasets
  Z.rf=randomForest(Zlessivage~.,data=train,na.action=na.exclude,importance=T,ntree=300,mtry=8)
  zimp=cbind(zimp,unname(importance(Z.rf))[,1])
  oobRsqr=c(oobRsqr,Z.rf$rsq[300])
  #test model against valdation datasets
  predicted=predict(Z.rf,newdata=test)
  observed=test$Zlessivage
  Rsqr=c(Rsqr,cov(observed,predicted)^2/(var(predicted)*var(observed)))
  RSME=c(RSME,((1/79)*sum((predicted-observed)^2))^.5)
}

##Variable importance analysis##################
#Summarize variable importance from 50 model runs from Z lessivage
zimp.summary=c()
for(i in 1:nrow(zimp)){
  zimp.summary=as.data.frame(rbind(zimp.summary,unname(summary(zimp[i,]))))
}
zimp.summary=as.data.frame(cbind(row.names(Z.rf$importance),zimp.summary))
colnames(zimp.summary)=c("IV","Min","Q1","Median","Mean","Q3","Max")
zimp.summary=zimp.summary[order(zimp.summary$Mean,decreasing=T),]
View(zimp.summary)

###SAVE Zimportance Data####
write.csv(zimp.summary,"Zimportance.csv", row.names=F)

#summarize out of box model error
summary(oobRsqr)

#summarize TRUE validation statistics
summary(Rsqr)
summary(RSME)
sd(Rsqr)
sd(RSME)


#Read in Data####################
#Removed depth to lessivage, taxonomic information and average clay content for each zone
Dinputs=data[,c(5,11:16,18:19,21:50)]
str(Dinputs)

##Tune for degree of lessivage for ntree and mtry##################
ntree=seq(100,1000,25)
nvariables=seq(2,20,1)

tuningresults=data.frame()

for(i in 1:length(ntree)){
  for(j in 1:length(nvariables)){
    tune=c(ntree[i],nvariables[j])
    set.seed(520)
    mod=randomForest(I.E.Ratio~.,data=Dinputs,na.action=na.exclude,ntree=tune[1],mtry=tune[2])
    temp=c(tune[1],tune[2],mod$mse[tune[1]])
    tuningresults=rbind(tuningresults,temp)
  }}
colnames(tuningresults)=c("ntree","mtry","mse")
plot(tuningresults$mse)

#View optimal ntree and mtry for degree of lessivage
subset(tuningresults,tuningresults$mse==min(tuningresults$mse))

##Random forest models for Dlessivage##################
#Run 50 random forest models for Dlessivage and store variable importances & R squareds
Dimp=c()
oobRsqr=c()
Rsqr=c()
RSME=c()
set.seed(2)
for(i in 1:50){
  #Randomly create calibration (80%) and validation (20%) datasets
  train=Dinputs[sample(nrow(Dinputs),316), ]
  test=Dinputs
  temp=unique(row.names(train))
  for(j in 1:nrow(train)){test=subset(test,row.names(test)!=temp[j])}
  #run model 50 times with unique cal and val datasets
  D.rf=randomForest(I.E.Ratio~.,data=train,na.action=na.exclude,importance=T,ntree=150, mtry=19)
  Dimp=cbind(Dimp,unname(importance(D.rf))[,1])
  oobRsqr=c(oobRsqr,D.rf$rsq[150])
  #test model against valdation datasets
  predicted=predict(D.rf,newdata=test)
  observed=test$I.E.Ratio
  Rsqr=c(Rsqr,cov(observed,predicted)^2/(var(predicted)*var(observed)))
  RSME=c(RSME,((1/79)*sum((predicted-observed)^2))^.5)
}

##Variable importance analysis##################
#summarize variable importance from 50 model runs from Z lessivage
Dimp.summary=c()
for(i in 1:nrow(Dimp)){
  Dimp.summary=as.data.frame(rbind(Dimp.summary,unname(summary(Dimp[i,]))))
}
Dimp.summary=as.data.frame(cbind(row.names(D.rf$importance),Dimp.summary))
colnames(Dimp.summary)=c("IV","Min","Q1","Median","Mean","Q3","Max")
Dimp.summary=Dimp.summary[order(Dimp.summary$Mean,decreasing=T),]
View(Dimp.summary)

###SAVE Dimportance Data####
write.csv(Dimp.summary,"Dimportance.csv", row.names=F)

#summarize out of box model error
summary(oobRsqr)

#summarize TRUE validation statistics
summary(Rsqr)
summary(RSME)
sd(Rsqr)
sd(RSME)


#Partial Dependency Plots##################
library(pdp)
Z.rf %>% #he %>% operator is read as "and then"
  partial(pred.var = "E_OC") %>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(f(E_OC)))



partial(Z.rf, pred.var = "E_OC", plot = TRUE, rug = TRUE)
partial(Z.rf, pred.var = "E_CEC", plot = TRUE, rug = TRUE)
partial(Z.rf, pred.var = "E_TEB", plot = TRUE, rug = TRUE)
partial(Z.rf, pred.var = "I_Porosity", plot = TRUE, rug = TRUE)
partial(Z.rf, pred.var = "I_DOMMIN", plot = TRUE, rug = TRUE)
partial(Z.rf, pred.var = "E_DOMMIN", plot = TRUE, rug = TRUE)
partial(Z.rf, pred.var = "Drainage.Class", plot = TRUE, rug = TRUE)
partial(Z.rf, pred.var = "Depth.to.Gleying", plot = TRUE, rug = FALSE)


partial(D.rf, pred.var="E_EA", plot=TRUE, rug = TRUE)
partial(D.rf, pred.var="E_Sand", plot=TRUE, rug = TRUE)
partial(D.rf, pred.var="I_BS", plot=TRUE, rug = TRUE)
partial(D.rf, pred.var="I_OC", plot=TRUE, rug = TRUE)
partial(D.rf, pred.var = "I_DOMMIN", plot = TRUE, rug = TRUE)
partial(D.rf, pred.var = "E_DOMMIN", plot = TRUE, rug = TRUE)
partial(D.rf, pred.var = "Depth.to.Gleying", plot = TRUE, rug = TRUE)


#ICE and c-ICE Plots##################
depth.ice <- partial(Z.rf, pred.var = "E_OC", ice = TRUE)
grid.arrange(
  autoplot(depth.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(depth.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

depth.ice <- partial(Z.rf, pred.var = "E_CEC", ice = TRUE)
grid.arrange(
  autoplot(depth.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(depth.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

depth.ice <- partial(Z.rf, pred.var = "E_TEB", ice = TRUE)
grid.arrange(
  autoplot(depth.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(depth.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

depth.ice <- partial(Z.rf, pred.var = "I_Porosity", ice = TRUE)
grid.arrange(
  autoplot(depth.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(depth.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

depth.ice <- partial(Z.rf, pred.var = "Depth.to.Gleying", ice = TRUE)
grid.arrange(
  autoplot(depth.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(depth.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)


degree.ice <- partial(D.rf, pred.var = "E_EA", ice = TRUE)
grid.arrange(
  autoplot(degree.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(degree.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

degree.ice <- partial(D.rf, pred.var = "I_Sand", ice = TRUE)
grid.arrange(
  autoplot(degree.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(degree.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

degree.ice <- partial(D.rf, pred.var = "E_Sand", ice = TRUE)
grid.arrange(
  autoplot(degree.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(degree.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

degree.ice <- partial(D.rf, pred.var = "I_BS", ice = TRUE)
grid.arrange(
  autoplot(degree.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(degree.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

degree.ice <- partial(D.rf, pred.var = "I_OC", ice = TRUE)
grid.arrange(
  autoplot(degree.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(degree.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)

degree.ice <- partial(D.rf, pred.var = "Depth.to.Gleying", ice = TRUE)
grid.arrange(
  autoplot(degree.ice, alpha = 0.1, train = data, rug = TRUE), # ICE curves
  autoplot(degree.ice, center = TRUE, alpha = 0.1, train = data, rug = TRUE), # c-ICE curves
  ncol = 2
)
