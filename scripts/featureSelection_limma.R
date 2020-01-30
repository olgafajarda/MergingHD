#!/usr/bin/env Rscript

# set the maximum number of nested expressions that will be evaluated
options("expressions"=5e5)

# load package limma
library(limma)

name_datasets="all"

# read the data frame with the expressions
nl<-scan(paste(name_datasets,"_id.txt",sep=""),what=character())
nc<-scan(paste(name_datasets,"_samples.txt",sep=""),what=character())
df<-read.table(paste(name_datasets,"_before_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df<-as.data.frame(df)

# read the class of every sample (D: diseased or Z: control)
tipo<-factor(scan(paste(name_datasets,"_type.txt",sep=""),what=character()))

# read the platform used to obtain the expression of a sample
platform<-factor(scan(paste(name_datasets,"_platform.txt",sep=""),what=character()))

# create the design matrix
design.matrix<-model.matrix(~0+tipo+platform)

# define a contrast matrix
contrast.matrix<-makeContrasts(Diff=tipoZ-tipoD,levels = design.matrix)

# fit the linear model
fit1<-lmFit(df,design.matrix)

# extract the linear model fit for the contrasts
fit2<-contrasts.fit(fit1,contrast.matrix)
fit3<-eBayes(fit2)

# write the results in the file "all_limma.txt"
write.csv2(topTable(fit3,coef='Diff',number=nrow(df)),file=paste(name_datasets,"_limma.txt",sep=""))

# obtain GBACC id of the different feasible sets
for(p in c(0.05, 0.01)){
  for(i in seq(1.5,3.0,0.1)){
    tab<-topTable(fit3,coef='Diff',number=nrow(df),p.value = p,lfc = log2(i))
    write(row.names(tab),file=paste("p",p*100,"fc",i*10,"_id.txt",sep=""))
  }
}
