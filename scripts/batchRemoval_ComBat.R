#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load packages Biobase and sva
library(Biobase)
library(sva)

# read the data
name_datasets="all"
nl<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
nc<-scan(paste(name_datasets,"_samples.txt",sep=""),what=character())
df<-read.table(paste(name_datasets,"_before_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
ex<-new("ExpressionSet",exprs=as.matrix(df))
edata<-exprs(ex)
df<-as.data.frame(t(df))

# read the platform used to obtain the expression of a sample
nl<-scan(paste(name_datasets,"_platform.txt",sep=""),what=character())
df["platform"]<-nl
df$platform=factor(df$platform)

# read the class of every sample (D: diseased or Z: control)
nl<-scan(paste(name_datasets,"_type.txt",sep=""),what=character())
df["type"]<-nl
df$type=factor(df$type)

# remove Batch
mod=model.matrix(~df$type)
batch=df$platform
batch=as.numeric(batch)
cleandata<-ComBat(edata,batch,mod)

# write data frame after removing batch to a text file
write.csv(cleandata, file=paste(name_datasets,"_after_rem_batch.txt",sep=""),sep="\t")

# write the different feasible sets
for(p in c(1)){
  for(i in c(15:26,28,30)){
    n<-scan(paste("p",p,"fc",i,"_id.txt",sep=""),what = character())
    index<-match(n,rownames(cleandata))
    write.table(cleandata[index,],file=paste("p",p,"fc",i,".txt",sep=""),sep = "\t",row.names = FALSE,col.names = FALSE)
  }
}
