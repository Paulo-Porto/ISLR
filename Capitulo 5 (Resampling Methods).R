
# LAB

# Validation Set

library(ISLR)
?sample
attach(Auto)
plot(horsepower,mpg)

set.seed(2)
train=sample(392,size=196)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
summary(lm.fit2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
summary(lm.fit3)

mean((mpg-predict(lm.fit,Auto))[-train]^2)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

erros.fit2=NA

for(i in 1:20){
  
  set.seed(i)
  train=sample(392,size=196)
  
  lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
  erros.fit2[i]=mean((mpg-predict(lm.fit2,Auto))[-train]^2)
  
}

plot(erros.fit2)
abline(h=mean(erros.fit2))

# Leave-One-Out Cross Validation

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

?cv.glm

cv.error=rep(0,5)
for(i in 1:5){
  
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta [1]
  
}

cv.error

# k-Fold Cross-Validation

set.seed(17)
cv.error=rep(0,10)
for(i in 1:10){
  
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit,K=10)$delta [1]
  
}

round(cv.error,digits=2) 

# Boostrap

library(boot)
library(ISLR)

?Portfolio

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,size=100,replace=TRUE))

# Usando a funcao boot
# Portfolio

?boot
boot(Portfolio,alpha.fn,R=1000)

# Usando a funcao boot
# Auto e um Modelo Linear

boot.fn=function(data,index ){
 return (coef(lm(mpg~horsepower,data=data,subset=index)))
}

boot.fn(Auto,1:392)
boot(Auto,boot.fn,2000)

summary(lm(Auto$mpg~Auto$horsepower,data=Auto))$coef

# Usando a funcao boot
# Auto e um Modelo Quadratico

boot.fn=function(data,index){
  return (coef(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index)))
}

boot.fn(Auto,1:392)
boot(Auto,boot.fn,1000)

summary(lm(Auto$mpg~Auto$horsepower+I(Auto$horsepower^2),data=Auto))$coef

# Exercicio 05

?Default
attach(Default)

#classificacao SEM a variavel estudante

erro.sem=NA

for(i in 1:1000){
  percent=0.75
  train=sample(length(default),size=percent*length(default),replace=F)
  
  log.fit=glm(default~balance+income,data=Default,family=binomial,subset=train)
  summary(log.fit)
  
  log.probs=predict(log.fit,Default[-train,],type="response")
  
  log.pred=rep("No",length(log.probs))
  log.pred[log.probs>0.5]="Yes"
  table(log.pred,default[-train])
  erro.sem[i]=1-mean(log.pred==default[-train])
}

plot(erro.sem)
abline(h=mean(erro.sem))

#classificacao COM a variavel estudante

erro.com=NA

for(i in 1:1000){
  percent=0.75
  train=sample(length(default),size=percent*length(default),replace=F)
  
  log.fit=glm(default~.,data=Default,family=binomial,subset=train)
  summary(log.fit)
  
  log.probs=predict(log.fit,Default[-train,],type="response")
  
  log.pred=rep("No",length(log.probs))
  log.pred[log.probs>0.5]="Yes"
  table(log.pred,default[-train])
  erro.com[i]=1-mean(log.pred==default[-train]) 
}

abline(h=mean(erro.com),col="red")

# O nivel do erro nao parece ter mudado

# Exercicio 06

library(ISLR)
attach(Default)

percent=0.75
train=sample(length(default),size=percent*length(default),replace=F)
  
log.fit=glm(default~balance+income,data=Default,family=binomial,subset=train)
summary(log.fit)

boot.fn=function(Data,index){
  return(summary(glm(default~balance+income,data=Data,family=binomial,subset=index))$coef[,2])
}

boot.fn(Default,train)

library(boot)

boot.fit=boot(Default[train,],boot.fn,1000)

boot.fit
summary(log.fit)$coef[,2]  # Os valores sao os mesmos!

# Exercicio 07 (LOOCV)

detach(Default)
attach(Weekly)

log.fit=glm(Direction~Lag1+Lag2,data=Weekly[-1,],family=binomial)
summary(log.fit)

predict(log.fit,Weekly[1,],type="response") # Previsao UP
Weekly[1,] # Dado real Down, erro na previsao

prob=NA
erros.loocv=NA

for(i in 1:(length(Direction))){
  
  log.fit=glm(Direction~Lag1+Lag2,data=Weekly[-i,],family=binomial)
  prob[i]=predict(log.fit,Weekly[i,],type="response")
  
  if(prob[i]>0.5) {prob[i]="Up"} else {prob[i]="Down"}
  
  if(Direction[i]!=prob[i]) {erros.loocv[i]=1} else {erros.loocv[i]=0}
  
}

mean(erros.loocv)

# Exercicio 08

set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

"n=100 e p = 2"

plot(x,y)
dados=data.frame(x,y)
erros.loocv=NA

for(i in 1:4){
  
  regression.fit=glm(y~poly(x,degree=i),data=dados)
  cv.err=cv.glm(dados,regression.fit)
  erros.loocv[i]=cv.err$delta[1]
  
}

plot(erros.loocv,type="l") # Com seed 1, 5, 50 e 500 nao houve mudanca

# O resultado e o mesmo e esperado nesse caso e porque o modelo 
# que gerou os dados e quadratico, entao espera-se que a regressao
# que considera o termo quadratico melhor explique os dados

regression.fit=glm(y~poly(x,degree=2),data=dados)
plot(regression.fit)

regression.fit=glm(y~poly(x,degree=4),data=dados)
summary(regression.fit) # corroborando que e significante ate o grau 02

# Exercicio 09

library(MASS)
attach(Boston)

plot(Boston)

plot(medv)
mu=mean(medv)
mu
abline(h=mu,col="red")

sd=sd(medv)/sqrt(length(medv))
sd

boot.fn=function(data,index){
  return(mean(data[index]))
}

library(boot)
boot.fit=boot(medv,boot.fn,1000)

boot.fit # Estimativa proxima do desvio padrao
mu
sd

boot.fit.interval=22.53281+1.96*c(-1,+1)*0.3999211

t.test(medv)
boot.fit.interval # intervalos bem similares

# Como os valores ficaram proximos, podemos considerar que a variavel
# 'media da medv' e suficiente proxima a uma distribuicao normal

boot.fn=function(data,index){
  return(median(data[index]))
}

boot.fit=boot(medv,boot.fn,1000)
boot.fit

hist(medv) # note que a variavel e assimetria a direita, logo a mediana
# e maior do que a media

abline(v=mean(medv),col="blue")
abline(v=median(medv),col="red")

# O desvio padrao da mediana e um pouco menor do que da media por Bootstrap

boot.fn=function(data,index){
  return(quantile(data[index],probs=0.10))
}

boot.fit=boot(medv,boot.fn,1000)
boot.fit
abline(v=quantile(medv,probs=0.10),col="green")

# O erro padrao e maior do que a media e mediana