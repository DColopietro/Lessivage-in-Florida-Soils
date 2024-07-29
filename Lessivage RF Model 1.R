#set working directory
setwd("")


data=data.frame(read.csv("Profile Database.csv",header=TRUE))
str(data)

 
View(colnames(data))

library(lattice)
library(randomForest)


#RandomForest model for depth of lessivage
#Removed degree of lessivage, taxonomic information and average % for mineralogy of each zone
Zinputs=data[,c(4,11:34,47:58)]
str(Zinputs)


set.seed(1)
Z.rf=randomForest(Zlessivage~.,data=Zinputs,importance=T,ntree=1000)


ZImp=importance(Z.rf)[,1]
ZImp=stack(data.frame(ZImp))
ZImp$regressor=rownames(importance(Z.rf))
ZImp=ZImp[order(ZImp$values,decreasing=T),]
ZImp$order=as.numeric(nrow(ZImp):1)
VIP_Z=dotplot(reorder(regressor,order)~values,data=ZImp,
    col="black",main="Zlessivage",xlab="% increase in MSE")
VIP_Z

 
Z.rf


#RandomForest model for degree of lessivage
#Removed depth to lessivage, taxonomic information and average % for mineralogy of each zone
Dinputs=data[,c(5,11:34,47:58)]
str(Dinputs)

 
set.seed(1)
D.rf=randomForest(I.E.Ratio~.,data=Dinputs,importance=T,ntree=1000)


DImp=importance(D.rf)[,1]
DImp=stack(data.frame(DImp))
DImp$regressor=rownames(importance(D.rf))
DImp=DImp[order(DImp$values,decreasing=T),]
DImp$order=as.numeric(nrow(DImp):1)
VIP_D=dotplot(reorder(regressor,order)~values,data=DImp,
    col="black",main="I.E.Ratio",xlab="% increase in MSE")
VIP_D

D.rf

#MSE plots for the randomforest models; depth of lessivage and degree of lessivage
data(Zinputs)
plot(randomForest(Zlessivage~., Zinputs, keep.forest=FALSE, ntree=1000), log="y", main="MSE of Zlessivage")
data(Dinputs)
plot(randomForest(I.E.Ratio~., Dinputs, keep.forest=FALSE, ntree=1000), log="y", main="MSE of I.E.Ratio")


ntree=seq(100,1000,25)
nvariables=seq(2,20,1)


tuningresults=data.frame()

for(i in 1:length(ntree)){
    for(j in 1:length(nvariables)){
        tune=c(ntree[i],nvariables[j])
        set.seed(520)
        mod=randomForest(Zlessivage~.,data=Zinputs,ntree=tune[1],mtry=tune[2])
        temp=c(tune[1],tune[2],mod$mse[tune[1]])
        tuningresults=rbind(tuningresults,temp)
}}
 
Zlessivage.results <- data.frame()

Zlessivage.modelinfo <- list()
Zlessivage.summary <- list()
Zlessivage.predictions <- list()

set.seed(100)
for(i in 2:37){
  splitIndex <- createDataPartition(Zinputs[,1], p = .8, list = FALSE, times = 50)
  trainDF <- Zinputs[ splitIndex,]
  testDF  <- Zinputs[-splitIndex,]
  
  objControl <- trainControl(method='cv', number=10)
  
  objModel <- caret::train(#data=trainDF,  #Doing it this 
                           #FEAMedian~.,   #way makes it create dummy variables for each factor variable and lets you see which variable within compname matter (in this case Rosenwall)
                           trainDF[,c(2:37)], trainDF[,1], 
                           method='gbm', 
                           #tuneGrid = gbmGrid,
                           trControl=objControl,  
                           metric = "RMSE")
                           
  
  Zlessivage.modelinfo[[length(Zlessivage.modelinfo)+1]] <- objModel
  Zlessivage.summary[[length(Zlessivage.summary)+1]] <-  summary(objModel)
  
  predictions <- predict(object=objModel, testDF[,c(2:37)], type='raw')

  
  compare_data <- cbind(testDF[1],predictions) 
  

  
  Zlessivage.results[i,1] <-  sqrt(mean((testDF$Zlessivage-predictions)^2))
  Zlessivage.results[i,2] <- mean(abs(testDF$Zlessivage-predictions))
  
  Zlessivage.results[i,3] <- summary(lm(testDF$Zlessivage~predictions))$adj.r.squared
  

  
  Zlessivage.predictions[[length(Zlessivage.predictions)+1]] <- compare_data
  
  
  
  Zlessivage.results[i,c(4:7)] <- objModel[6]$bestTune

  print(i)
}

colnames(Zlessivage.results)[1:5] <- c("RMSE","MAE","adj.R.sqrd",
                                          "n.trees","interactionDepth")


save(Zlessivage.results, Zlessivage.modelinfo,
     Zlessivage.summary, Zlessivage.predictions,
     file="Z:/Florida State Soil Characterization/Lessivage/Zlessivage.rdata")

ZlessivageSummary <- data.frame()

ZlessivageSummary <- Zlessivage.summary[[1]]
for (i in 2:37){
  ZlessivageSummary<- rbind(ZlessivageSummary,Zlessivage.summary[[1]])
  
}

Zlessivage.summary.vars <- ZlessivageSummary %>% group_by(var) %>% summarize(average_rel_inf= mean(rel.inf),
                                                                       sigma_rel_inf= sd(rel.inf),
                                                                       min_rel_inf= min(rel.inf),
                                                                       max_rel_inf= max(rel.inf))

Zlessivage.summary.results <- Zlessivage.results %>% summarize(average_RMSE= mean(RMSE),
                                                              sigma_RMSE= sd(RMSE),
                                                              average_MAE= mean(MAE),
                                                              sigma_MAE= sd(MAE),
                                                              average_adj.R.sqrd= mean(adj.R.sqrd),
                                                              sigma_adj.R.sqrd= sd(adj.R.sqrd))


View(Zlessivage.summary.vars)


FEApredresults <- data.frame()

FEApredresults <- FEAMedian_gbm_Predictions[[1]]
for (i in 2:50){
  FEApredresults <- rbind(FEApredresults,FEAMedian_gbm_Predictions[[i]])
  
}



plot(FEApredresults$FEAMedian,FEApredresults$predictions, main="Results 50 GBM runs with 1:1 Line",
     xlab="Observed Median Microaggregate Size (um)", ylab="Predicted Median Aggregate Size (um)", 
     xlim=c(0,800), ylim=c(0,800))
abline(0,1)


FEA_Median_Results 
abline(0,1)


