#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

#load package caret
library(caret)

# read the data frame with the expressions
name_datasets="all"
nl<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
nc<-scan(paste(name_datasets,"_samples.txt",sep=""),what=character())
df<-read.table(paste(name_datasets,"_before_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df1<-read.table(paste(name_datasets,"_after_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df<-as.data.frame(t(df))
df1<-as.data.frame(t(df1))

# read the class of every sample (D: diseased or Z: control)
tipo<-factor(scan(paste(name_datasets,"_type.txt",sep=""),what=character()))
df["type"]<-tipo
df1["type"]<-tipo

# read the platform used to obtain the expression of a sample
platform<-factor(scan(paste(name_datasets,"_platform.txt",sep=""),what=character()))
df["platform"]<-platform

#divide the dataset into a training (70% of the samples) and a test set (30% of the samples)
for(i in 1:30){
  set.seed(i*7)
  inTraining <- createDataPartition(df$type, p = .70, list = FALSE)
  train_limma<-df[inTraining,]
  train_rf<-df1[inTraining,]
  test<-df1[-inTraining,] 
  
  write.table(train_rf$type,file = paste("HDtrain",i,"_type.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(test$type,file = paste("HDtest",i,"_type.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  train_limma$type<-NULL
  train_rf$type<-NULL
  test$type<-NULL
  
  write.table(train_limma$platform,paste("HDtrain",i,"_platform.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE) 
  train_limma$platform<-NULL

  write.table(train_limma,file = paste("HDtrain",i,"_before.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(train_rf,file = paste("HDtrain",i,"_after.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(test,file = paste("HDtest",i,".txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(row.names(train_rf),file = paste("HDtrain",i,"_samples.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
  write.table(row.names(test),file = paste("HDtest",i,"_samples.txt",sep=""),col.names = FALSE, sep="\t",row.names = FALSE)
}
