#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages randomForest, caret, MLmetrics, plyr and mltools
library(randomForest)
library(caret)
library(MLmetrics)
library(plyr)
library(mltools)

# Data frame with the accuracy results of every feature set
resultsACC<-data.frame()
# Data frame with the MCC results of every feature set
resultsMCC<-data.frame()
# Data frame with the ROC results of every feature set
resultsROC<-data.frame()
# Data frame with the AUC results of every feature set
resultsAUC<-data.frame()

# Tune the parameters mtry and ntree
name_datasets=c("p1fc15","p1fc16","p1fc17","p1fc18","p1fc19","p1fc20","p1fc21","p1fc22","p1fc23","p1fc24","p1fc25","p1fc26","p1fc28","p1fc30")
for(na in name_datasets){
  accuracy=0
  nl<-scan(paste(na,"_id.txt",sep=""),what=character())
  maxnumber=length(nl)
  nc<-scan("all_samples.txt",what=character())
  df<-read.table(paste(na,".txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
  df<-as.data.frame(t(df))
  nl<-scan("all_type.txt",what=character())
  df["type"]<-nl
  df$type=factor(df$type)
  for (j in c(125,250,375,500,625,750,875,1000)){
    notree=j
    nmtry=2
    set.seed(7)
    control<-trainControl(method="repeatedcv", number=10,repeats=3,search="grid")
    tunegrid<-expand.grid(.mtry=nmtry)
    rfn<-train(type~.,data=df,method="rf",metric="Accuracy", tuneGrid =tunegrid, trControl=control,importance=TRUE, ntree=notree)
    tab<-as.data.frame(rfn$results)
    tab["ntree"]<-c(notree)
    for (i in 3:maxnumber){
      nmtry=i
      set.seed(7)
      control<-trainControl(method="repeatedcv", number=10,repeats=3,search="grid")
      tunegrid<-expand.grid(.mtry=nmtry)
      rfn<-train(type~.,data=df,method="rf",metric="Accuracy", tuneGrid =tunegrid, trControl=control,importance=TRUE, ntree=notree)
      tabx<-as.data.frame(rfn$results)
      tabx["ntree"]<-c(notree)
      tab<-rbind(tab,tabx) 
    }
    tab2<-tab[order(-tab$Accuracy),]
    if(tab2$Accuracy[1]>accuracy){
      accuracy=tab2$Accuracy[1]
      notree2=j
      nmtry2=tab2$mtry[1]
    }
    write.csv2(tab,file=paste(na,"_",j,"_randomFoest_results_tuning_without.txt",sep=""))
  }
  
  # Get and write the results of the model
  control<-trainControl(method="repeatedcv", number=10,repeats=3,search="grid",savePred=T, classProb=T)
  tunegrid2<-expand.grid(mtry=nmtry2)
  set.seed(7)
  rfn<-train(type~.,data=df,method="rf",metric="Accuracy", trControl=control, tuneGrid=tunegrid2, importance=TRUE, ntree=notree2)
  tabx<-as.data.frame(rfn$results)
  tabx["ntree"]<-c(notree2)
  tabx["na"]<-c(na)
  tabx<-tabx[c("na","mtry","ntree","Accuracy","AccuracySD","Kappa","KappaSD")]
  resultsACC<-rbind(resultsACC,tabx)
  write.csv2(varImp(rfn,scale=FALSE)$importance, file=paste(na,"_varImp_without.txt",sep=""))
  
  MCC_metric<-ddply(rfn$pred,"Resample",summarise, MCC=mcc(pred,obs))
  media<-mean(MCC_metric$MCC)
  dsvp<-sd(MCC_metric$MCC)
  tabx<-data.frame(na=na,mtry=nmtry2,ntree=notree2,MCCmean=media,MCCSD=dsvp)
  resultsMCC<-rbind(resultsMCC,tabx)
  
  set.seed(7)
  control<-trainControl(method="repeatedcv", number=10,repeats=3,search="grid",classProbs = TRUE, summaryFunction = twoClassSummary)
  rfROC<-train(type~.,data=df,method="rf",metric="ROC", tuneGrid =tunegrid2, trControl=control,importance=TRUE,ntree=notree2)
  tabx<-as.data.frame(rfROC$results)
  tabx["ntree"]<-c(notree2)
  tabx["na"]<-c(na)
  tabx<-tabx[c("na","mtry","ntree","ROC","ROCSD","Spec","SpecSD","Sens","SensSD")]
  resultsROC<-rbind(resultsROC,tabx)
  
  set.seed(7)
  control<-trainControl(method="repeatedcv", number=10,repeats=3,search="grid",classProbs = TRUE, summaryFunction = prSummary)
  rfAUC<-train(type~.,data=df,method="rf",metric="AUC", tuneGrid =tunegrid2, trControl=control,importance=TRUE,ntree=notree2)
  tabx<-as.data.frame(rfAUC$results)
  tabx["ntree"]<-c(notree2)
  tabx["na"]<-c(na)
  tabx<-tabx[c("na","mtry","ntree","Precision","PrecisionSD","Recall","RecallSD","F","FSD","AUC","AUCSD")]
  resultsAUC<-rbind(resultsAUC,tabx)
}

write.csv2(resultsACC,file="Results_Accuracy_withou.txt")
write.csv2(resultsMCC,file="Results_MCC_without.txt")
write.csv2(resultsROC,file="Results_ROC_without.txt")
write.csv2(resultsAUC,file="Results_AUC_without.txt")
