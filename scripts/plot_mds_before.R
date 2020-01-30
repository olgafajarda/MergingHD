#!/usr/bin/env Rscript

# read the data frame with the expressions
nome="all"
nl<-scan(paste(nome,"_id.txt",sep=""),what=character())
nc<-scan(paste(nome,"_samples.txt",sep=""),what=character())
df<-read.table(paste(nome,"_before_rem_batch.txt",sep=""),header=FALSE,sep="\t",dec=".",row.names=nl,col.names=nc)
df<-as.data.frame(t(df))

# read the class of every sample (D: diseased or Z: control)
tipo<-factor(scan(paste(nome,"_type.txt",sep=""),what=character()))

# read the platform used to obtain the expression of a sample
platform<-factor(scan(paste(nome,"_platform.txt",sep=""),what=character()))

# calculate the distances
d <- dist(df)
mds<-cmdscale(d)

# chose the colours and shapes of the points
cols=c('red', 'blue', 'black', 'green')
shps = c(15, 16)

# plot mds
par(mar=c(5.1, 4.1, 4.1, 22.4), xpd=TRUE)
plot(mds, col=cols[platform], pch=shps[tipo], asp=1, xlab = "", ylab = "", main="MDS plot before batch-adjustment") 
legend("topright", col=cols, inset=c(-0.25,0), legend=levels(platform), pch = 16, cex = 0.9, title="Platforms")
legend('right', legend=levels(tipo), pch = shps, cex = 1, inset=c(-0.26,0), title="Samples' type")

