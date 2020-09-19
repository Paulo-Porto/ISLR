
# Lab 01 PCA

?USArrests
states=row.names(USArrests)

plot(USArrests)
names(USArrests)

apply(USArrests,2,mean)
apply(USArrests,2,var)

pca.fit=prcomp(USArrests,scale=TRUE)

names(pca.fit)
# The center and scale components correspond to the means and standard
# deviations of the variables that were used.

pca.fit$rotation
pca.fit$x

biplot(pca.fit,scale=0)

pca.var=pca.fit$sdev^2
pca.pve=pca.var/sum(pca.var)

plot(pca.pve,xlab="Principal Component",ylab="% of Variance",
     ylim=c(0,1),type="b")

plot(cumsum(pca.pve),xlab="Principal Component",ylab="% of Cumulative Variance",
     ylim=c(0,1),type="b")

# Lab 03 NCI60 Data Example

library(ISLR)
?NCI60

nci.labs=NCI60$labs
nci.data=NCI60$data

table(nci.labs)

pca.fit=prcomp(nci.data,scale=TRUE)

cols=function(vector){ # Funcao util para visualizacao de dados
  cols=rainbow(length(unique(vector)))
  
  return(cols[as.numeric(as.factor(vector))])
}

par(mfrow=c(1,2))
plot(pca.fit$x[,1:2],col=cols(nci.labs),pch=19,xlab="Z1",ylab="Z2")
plot(pca.fit$x[,c(1,3)],col=cols(nci.labs),pch=19,xlab="Z1",ylab="Z3")
par(mfrow=c(1,1))
# On the whole, observations belonging to a single cancer type tend to
# lie near each other in this low-dimensional space. It would not have 
# been possible to visualize the data without using a dimension reduction 
# method such as PCA


summary(pca.fit)
plot(pca.fit)

summary(pca.fit)$importance[2,] # PVE
summary(pca.fit)$importance[3,] # PVE Acumulado

pca.var=pca.fit$sdev^2
pca.pve=pca.var/sum(pca.var)

par(mfrow=c(1,2))
plot(100*pca.pve,xlab="Principal Component",ylab="% of Variance",
     type="o",col="blue")
plot(cumsum(pca.pve),xlab="Principal Component",
     ylab="% of Cumulative Variance",type="o",col="brown3")
par(mfrow=c(1,1))
