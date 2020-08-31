
install.packages("LiblineaR")
library(LiblineaR)

# Exemplo 01 - Fronteira nao Linear

set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1

plot(x,col=(3-y))

dat=data.frame(x=x,y=as.factor(y))
library(e1071)

svm.fit=svm(y~.,data=dat,kernel="linear",cost=10,
            scale=FALSE) # Pensar em cada caso se vale a pena usar scale

plot(svm.fit,dat)
# The support vectors are plotted as crosses
# and the remaining observations are plotted as circles

summary(svm.fit)
# Unfortunately,the svm() function does not explicitly output the 
# coefficients of the linear decision boundary obtained when the support 
# vector classifier is fit, nor does it output the width of the margin.

set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",
              range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
# The e1071 library includes a built-in function, tune(), to perform 
# crosstune() validation. By default, tune() performs ten-fold 
# cross-validation on a set of models of interest

summary(tune.out)

bestmodel=tune.out$best.model
summary(bestmodel)

set.seed(1)
xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1),20,replace=T)
xtest[ytest==1,]=xtest[ytest==1,]+1

testdat=data.frame(x=xtest,y=as.factor(ytest))

ypred=predict(bestmodel,testdat)
table(predict=ypred,truth=testdat$y)

# Exemplo 02 - Fronteira Linear

x[y==1,]=x[y==1,]+0.5
plot(x,col=(y+5)/2,pch=19)

dat=data.frame(x=x,y=as.factor(y))

svm.fit=svm(y~.,data=dat,kernel="linear",cost=1e5,
            scale=FALSE) # Pensar em cada caso se vale a pena usar scale
summary(svm.fit)
plot(svm.fit,dat)

svm.fit=svm(y~.,data=dat,kernel="linear",cost=1,
            scale=FALSE) # Pensar em cada caso se vale a pena usar scale
summary(svm.fit)
plot(svm.fit,dat)

# Exemplo 03 - SVM

set.seed(1)
x=matrix(rnorm(200*2),ncol=2)

x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2

y=c(rep(1,150),rep(2,50))

dat=data.frame(x=x,y=as.factor(y))

plot(x,col=y)

train=sample(200,100)
svm.fit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svm.fit,dat[train,])
summary(svm.fit)

svm.fit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svm.fit,dat[train,])

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
              gamma=c(0.5,1,2,3,4)))
summary(tune.out)

table(true=dat[-train,"y"],
      pred=predict(tune.out$best.model,newdata=dat[-train,]))

mean(dat[-train,"y"]==predict(tune.out$best.model,newdata=dat[-train,]))
# proporcao de acertos

# Exemplo 04 - Curvas ROC

install.packages("ROCR")
library(ROCR)

rocplot=function(pred,truth,...){
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...)
}

# Melhor Modelo

svm.fit=svm(y~.,data=dat[train,],kernel="radial",
        gamma=0.5,cost=1,decision.values=T)
fitted=attributes(predict(svm.fit,
       dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],main="Training Data")

# Modelo Flexivel

svm.fit=svm(y~.,data=dat[train,],kernel="radial",
            gamma=50,cost=1,decision.values=T)
fitted=attributes(predict(svm.fit,
            dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")

# Test Error - Melhor Modelo

svm.fit=svm(y~.,data=dat[train,],kernel="radial",
            gamma=0.5,cost=1,decision.values=T)
fitted=attributes(predict(svm.fit,dat[-train,],
            decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")

# Test Error - Modelo Flexivel

svm.fit=svm(y~.,data=dat[train,],kernel="radial",
            gamma=50,cost=1,decision.values=T)
fitted=attributes(predict(svm.fit,dat[-train,],
                          decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

# Applied - 04

x=matrix(rnorm(n=2*100,mean=4,sd=1),ncol=2)
y=rbinom(n=100,size=1,prob=0.5)
x[y==0,]=sqrt(x[y==0,])+2
plot(x,col=y+1)

train=sample(nrow(x),nrow(x)/2,replace=F)

dat=data.frame(x=x,y=as.factor(y))

# Modelo Linear

svm.fit=svm(y~.,data=dat[train,],kernel="linear",cost=5,
            scale=FALSE) # Pensar em cada caso se vale a pena usar scale

plot(svm.fit,dat[train,])

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="linear",
              range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

ypred=predict(svm.fit,dat[-train,])
table(predict=ypred,truth=dat[-train,"y"])
mean(ypred==dat[-train,"y"]) # 50% de acerto

# Modelo Radial

svm.fit=svm(y~.,data=dat[train,],kernel="radial",gamma=0.5,cost=1)
plot(svm.fit,dat[train,])

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4)))
summary(tune.out)

ypred=predict(svm.fit,dat[-train,])
table(predict=ypred,truth=dat[-train,"y"])
mean(ypred==dat[-train,"y"]) # 84% de acerto

# Modelo Polinomial

svm.fit=svm(y~.,data=dat[train,],kernel="polynomial",
            gamma=2,cost=0.1,degree=4)
plot(svm.fit,dat[train,])

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="polynomial",
              ranges=list(cost=c(0.1,1,10),
              gamma=c(0.5,2,4),degree=c(3,4,5)))
summary(tune.out)

ypred=predict(svm.fit,dat[-train,])
table(predict=ypred,truth=dat[-train,"y"])
mean(ypred==dat[-train,"y"]) # 88% de acerto

# Applied - 05

x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2>0)

plot(x1,x2,col=y+2,pch=19,main="Dados Gerados")

base=data.frame(x1,x2,y=as.factor(y))
train=1:300

log.fit=glm(y~x1+x2,data=base[train,],family=binomial)
log.probs=predict(log.fit,base[-train,-3],type="response")

log.pred=rep(0,length(log.probs))
log.pred[log.probs>0.5]=1

mean(log.pred==base[-train,"y"]) # % Acerto
table(log.pred,base[-train,"y"])

plot(x1[-train],x2[-train],col=log.pred+2,pch=19,
     main="Previsao Regressao Logistica no Test Set")

# Transformacoes nao Lineares

log.fit=glm(y~poly(x1,degree=2)+poly(x2,degree=3),data=base[train,],family=binomial)
log.probs=predict(log.fit,base[-train,-3],type="response")

log.pred=rep(0,length(log.probs))
log.pred[log.probs>0.5]=1

plot(x1[-train],x2[-train],col=log.pred+2,pch=19,
     main="Regressao Logistica com Transformacao nao Linear")
mean(log.pred==base[-train,"y"]) # % Acerto
table(log.pred,base[-train,"y"])

# Trabalhando com SVM

dat=data.frame(x1,x2,y=as.factor(y))
svm.fit=svm(y~.,data=dat[train,],kernel="polynomial",
            gamma=1,cost=10,degree=2)
plot(svm.fit,dat[train,])

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="polynomial",
              ranges=list(cost=c(0.1,1,10),
                          gamma=c(1,2,3),degree=c(2,3,4)))
summary(tune.out)

ypred=predict(svm.fit,dat[-train,])
table(predict=ypred,truth=dat[-train,"y"])
mean(ypred==dat[-train,"y"]) # % de acerto

# A transformacao nao linear da regressao logistica conseguiu 
# resultado bem similar ao SVM

# Applied - 07

# Modelo Linear

library(ISLR)
?Auto

binario=rep(0,times=nrow(Auto))
binario[Auto$mpg>median(Auto$mpg)]=1
dat=data.frame(Auto,y=as.factor(binario))

prop_train=0.60
set.seed(1)
train=sample(x=nrow(Auto),size=prop_train*nrow(Auto),replace=F)

svm.fit=svm(y~.,data=dat[train,],kernel="linear",cost=1,
            scale=FALSE)

plot(dat$mpg[train],dat$cylinders[train],col=y+2,pch=19)
# Notem que a separacao e dificil
plot(svm.fit,dat[train,],cylinders~mpg)

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="linear",
              range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

ypred=predict(svm.fit,dat[-train,])
table(predict=ypred,truth=dat[-train,"y"])
mean(ypred==dat[-train,"y"]) # % de acerto

# Modelo Polinomial

svm.fit=svm(y~.,data=dat[train,],kernel="polynomial",
            gamma=0.5,cost=1,degree=3)

plot(svm.fit,dat[train,],cylinders~mpg)

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="polynomial",
              ranges=list(cost=c(0.1,1,10),
                          gamma=c(0.5,2,4),degree=c(3,4,5)))
summary(tune.out)

ypred=predict(svm.fit,dat[-train,])
table(predict=ypred,truth=dat[-train,"y"])
mean(ypred==dat[-train,"y"]) # % de acerto

# Modelo Radial

svm.fit=svm(y~.,data=dat[train,],kernel="radial",gamma=0.5,cost=1)

plot(svm.fit,dat[train,],cylinders~mpg)

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4)))
summary(tune.out)

ypred=predict(svm.fit,dat[-train,])
table(predict=ypred,truth=dat[-train,"y"])
mean(ypred==dat[-train,"y"]) # % de acerto

# Os modelos tem um otimo nivel de acerto, que nao esta sendo bem visto
# nos graficos, mas aparentemente existe uma separacao clara na variavel
# de interesse quando a analisamos do ponto de vista multivariado

# Applied - 08

# Modelo Linear

library(ISLR)
?OJ

set.seed(1)
train=sample(x=nrow(OJ),size=800,replace=F)

svm.fit=svm(Purchase~.,data=OJ[train,],kernel="linear",cost=0.1,
            scale=FALSE)
summary(svm.fit)

set.seed(1)
tune.out=tune(svm,Purchase~.,data=OJ[train,],kernel="linear",
              range=list(cost=c(0.001,0.01,0.1,1,5,10)))
summary(tune.out)

ypred=predict(svm.fit,OJ[-train,])
table(predict=ypred,truth=OJ[-train,"Purchase"])
mean(ypred==OJ[-train,"Purchase"]) # % de acerto

# Modelo Radial

svm.fit=svm(Purchase~.,data=OJ[train,],kernel="radial",cost=1,
            scale=FALSE)

set.seed(1)
tune.out=tune(svm,Purchase~.,data=OJ[train,],kernel="radial",
              range=list(cost=c(0.001,0.01,0.1,1,5,10)))
summary(tune.out)

ypred=predict(svm.fit,OJ[-train,])
table(predict=ypred,truth=OJ[-train,"Purchase"])
mean(ypred==OJ[-train,"Purchase"]) # % de acerto

# Modelo Polynomial

svm.fit=svm(Purchase~.,data=OJ[train,],kernel="polynomial",cost=1,
            scale=FALSE,degree=2)

set.seed(1)
tune.out=tune(svm,Purchase~.,data=OJ[train,],kernel="polynomial",
              range=list(cost=c(0.001,0.01,0.1,1,5,10)))
summary(tune.out)

ypred=predict(svm.fit,OJ[-train,])
table(predict=ypred,truth=OJ[-train,"Purchase"])
mean(ypred==OJ[-train,"Purchase"]) # % de acerto

# Nesse cenário o modelo polinomial foi um pouco melhor do que o linear