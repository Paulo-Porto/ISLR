
# Como ler as árvores

# At a given internal node, the label (of the form Xj < tk) indicates the
# left-hand branch emanating from that split, and the right-hand branch 
# corresponds to Xj > tk.

# 8.3.1 Fitting Classification Trees

library(tree)
library(ISLR)

High=ifelse(Carseats$Sales<=8,"No","Yes")
High=as.factor(High)

dados=data.frame(Carseats[,-1],High)
attach(dados)

tree.fit=tree(High~.,dados)
summary(tree.fit)

plot(tree.fit)
?text
text(tree.fit,pretty=0)

tree.fit

# Training Set Cross Validation

parcela.test=0.50
set.seed(2)
test=sample(1:length(High),length(High)*parcela.test)
dados.test=dados[test,]

tree.fit=tree(High~.,data=dados,subset=-test)
tree.pred=predict(tree.fit,dados.test,type="class")

table(tree.pred,dados.test$High)
mean(tree.pred==dados.test$High)

set.seed(3)
cv.fit=cv.tree(tree.fit,FUN=prune.misclass)
# argument FUN=prune.misclass in order to indicate that we want the classification
# error rate to guide the cross-validation and pruning process, rather than 
# the default for the cv.tree() function, which is deviance

# despite the name, dev corresponds to the cross-validation error rate

cv.fit

par(mfrow=c(1,2))

plot(cv.fit$size,cv.fit$dev,type="b")
plot(cv.fit$k,cv.fit$dev,type="b")

par(mfrow=c(1,1))

prune.fit=prune.misclass(tree.fit,best=5) # Eu escolheria best = 5
plot(prune.fit)
text(prune.fit,pretty=0)

tree.pred=predict(prune.fit,dados.test,type="class")

table(tree.pred,dados.test$High)
mean(tree.pred==dados.test$High)

# 8.3.2 Fitting Regression Trees

library(MASS)

set.seed(1)
parcela.train=0.80
train=sample(1:nrow(Boston),nrow(Boston)*parcela.test)

tree.boston=tree(medv~.,data=Boston,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")
# Ja estamos em um bom tamanho

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"] # Jeito interessante de indicar colunas

plot(yhat,boston.test)
abline(0,1)

MSE=mean((yhat-boston.test)^2)
sqrt(MSE) # As previsoes ficam em torno desse valor do real parametro


# 8.3.3 Bagging and Random Forests

#install.packages("randomForest")
library(randomForest)

# Recall that bagging is simply a special case of a random forest with m = p.

# Bagging
 
set.seed(1)
?randomForest
bag.boston=randomForest(medv~.,Boston,subset=train,mtry=ncol(Boston)-1,importance=T)

bag.boston

yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)

MSE=mean((yhat.bag-boston.test)^2)
sqrt(MSE)

# Random Forest

set.seed(1)
random.boston=randomForest(medv~.,Boston,subset=train,importance=T)
# Removendo o argumento mtry

random.boston

yhat.random=predict(random.boston,newdata=Boston[-train,])
plot(yhat.random,boston.test)
abline(0,1)

MSE=mean((yhat.random-boston.test)^2)
sqrt(MSE) # As previsoes ficam em torno desse valor do real parametro

importance(random.boston)
varImpPlot(random.boston)

# Nao e mais possivel plotar os dados

# 8.3.4 Boosting

#install.packages("gbm")
library(gbm)

# We run gbm() with the option distribution="gaussian" since this is a regression 
# problem; if it were a binary classification problem, we would use distribution="bernoulli".

set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],
                 distribution="gaussian", n.trees=5000,interaction.depth=4)

summary(boost.boston)

# These plots partial dependence plot illustrate the marginal effect of the
# selected variables on the response after integrating out the other variables

plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost=predict(boost.boston,newdata=Boston[-train,])
plot(yhat.boost,boston.test)
abline(0,1)

MSE=mean((yhat.boost-boston.test)^2)
sqrt(MSE) # As previsoes ficam em torno desse valor do real parametro

# Testando o parametro lambda

lambda=NA
Erro=NA

for(i in seq(from=0.001,to=0.02,by=0.001)){
  
  boost.boston=gbm(medv~.,data=Boston[train,], distribution="gaussian", 
        n.trees=5000, interaction.depth=4, shrinkage=i, verbose=FALSE)
  
  yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
  
  lambda[i/0.001]=i
  Erro[i/0.001]=mean((yhat.boost-boston.test)^2)
  
}

plot(lambda,Erro)
abline(h=min(Erro),col="red") # Lambda = 0.017

# Applied

# Recall that bagging is simply a special case of a random forest with m = p.

erro.p=NA
erro.p2=NA
erro.p.raiz=NA

p=ncol(Boston)-1

# Modelo com mtry = P

for (i in seq(from=100,to=5000,by=100)){
  
  rf.boston=randomForest(medv~.,Boston,subset=train,mtry=p,
                         importance=T, ntree=i)  
  
  yhat.rf=predict(rf.boston,newdata=Boston[-train,])
  
  erro.p[i/100]=mean((yhat.rf-boston.test)^2)

}

plot(seq(from=100,to=5000,by=100),erro.p,type="l",col=1)

# Modelo com mtry = P/2

for (i in seq(from=100,to=5000,by=100)){
  
  rf.boston=randomForest(medv~.,Boston,subset=train,
                         mtry=round(p/2,digits=0),importance=T,ntree=i)  
  
  yhat.rf=predict(rf.boston,newdata=Boston[-train,])
  
  erro.p2[i/100]=mean((yhat.rf-boston.test)^2)
  
}

plot(seq(from=100,to=5000,by=100),erro.p2,type="l",col=2)

# Modelo com mtry = Raiz de P

for (i in seq(from=100,to=5000,by=100)){
  
  rf.boston=randomForest(medv~.,Boston,subset=train,
                         mtry=round(sqrt(p),digits=0),importance=T,
                         ntree=i)  
  
  yhat.rf=predict(rf.boston,newdata=Boston[-train,])
  
  erro.p.raiz[i/100]=mean((yhat.rf-boston.test)^2)
  
}

plot(seq(from=100,to=5000,by=100),erro.p.raiz,type="l",col=3)

# Consolidando os graficos

matplot(cbind(erro.p,erro.p2,erro.p.raiz),type="l",verbose=F)
# https://pt.stackoverflow.com/questions/85844/sobrepor-gr%C3%A1fico-no-r

# As florestas aleatorias com parametro igual raiz p foram consideravalmente
# melhores em reduzir o MSE

detach(dados)

# Exercicio 08

# Abordagem de Arvore de Regressao

attach(Carseats)
parcela.train=0.80
set.seed(10)
train=sample(1:nrow(Carseats),nrow(Carseats)*parcela.test)
test.sales=Carseats[-train,"Sales"] # Jeito interessante de indicar colunas

tree.fit=tree(Sales~.,data=Carseats,subset=train)
yhat=predict(tree.fit,newdata=Carseats[-train,])
mean((yhat-test.sales)^2) # Taxa de Erro de 5.20

cv.fit=cv.tree(tree.fit)
par(mfrow=c(1,2))
plot(cv.fit$size,cv.fit$dev,type="b")
plot(cv.fit$k,cv.fit$dev,type="b")
par(mfrow=c(1,1))

cv.fit$size[cv.fit$dev==min(cv.fit$dev)] # Vamor reduzir para 06
cv.fit$k[cv.fit$dev==min(cv.fit$dev)] # Vamos escolher K = 38.50

prune.fit=prune.tree(tree.fit,best=6,k=38.5)
plot(prune.fit)
text(prune.fit,pretty=0)

yhat=predict(prune.fit,newdata=Carseats[-train,])
mean((yhat-test.sales)^2) # Taxa de Erro de 5.13

# Abordagem de Bagging

set.seed(1)
bag.fit=randomForest(Sales~.,data=Carseats,subset=train,mtry=ncol(Carseats)-1,importance=T)
bag.fit

yhat.bag=predict(bag.fit,newdata=Carseats[-train,])
plot(yhat.bag,Carseats[-train,"Sales"])
abline(0,1)

MSE=mean((yhat.bag-Carseats[-train,"Sales"])^2)
sqrt(MSE) # Diminuiu MUITO o MSE

importance(bag.fit)
varImpPlot(bag.fit)

# Abordagem de Random Florests

p=ncol(Carseats)-1

erro.p=NA
erro.p2=NA
erro.p.raiz=NA

for(i in c(p,p/2,sqrt(p))){
  
  set.seed(1)
  rf.fit=randomForest(Sales~.,Carseats,subset=train,mtry=i,importance=T)
  yhat.rf=predict(rf.fit,newdata=Carseats[-train,])
  
  if(i==p){erro.p=sqrt(mean((yhat.rf-Carseats[-train,"Sales"])^2))} 
  if(i==p/2){erro.p2=sqrt(mean((yhat.rf-Carseats[-train,"Sales"])^2))} 
  if(i==sqrt(p)){erro.p.raiz=sqrt(mean((yhat.rf-Carseats[-train,"Sales"])^2))  }
  
}

MSE=round(matrix(c(erro.p,erro.p2,erro.p.raiz),nrow=1),digits=2)
barplot(MSE,names.arg=c("p = 1.72","p/2 = 1.70","raiz de p = 1.75"))

detach(Carseats)

# Exercicio 09

?OJ
attach(OJ)

set.seed(1)
train=sample(nrow(OJ),size=800)

tree.fit=tree(Purchase~.,data=OJ,subset=train)
summary(tree.fit)
# Arvore com 09 nos terminais e Train Error = 15.9%

plot(tree.fit)
text(tree.fit)
# A lealdade a marca CH parece a variavel mais importante

tree.predict=predict(tree.fit,newdata=OJ[-train,],type="class")
table(tree.predict,OJ[-train,]$Purchase)
1-mean(tree.predict==OJ[-train,]$Purchase) # Test Error = 17.7%

# Pruning the Tree

set.seed(1)
cv.fit=cv.tree(tree.fit,FUN=prune.misclass)
cv.fit

par(mfrow=c(1,2))
plot(cv.fit$size,cv.fit$dev,type="b")
plot(cv.fit$k,cv.fit$dev,type="b")
par(mfrow=c(1,1))

prune.fit=prune.misclass(tree.fit,best=4)
plot(prune.fit)
text(prune.fit,pretty=0)

tree.predict=predict(prune.fit,newdata=OJ[-train,],type="class")

table(tree.predict,OJ[-train,]$Purchase)
1-mean(tree.predict==OJ[-train,]$Purchase) # Test Error = 19.6%

# Aumentou um pouco o erro, mas acho que a simplicidade compensa
detach(OJ)

# Exercicio 10

?Hitters
base=na.omit(Hitters)
hist(base$Salary) # Antes da transformacao
base$Salary=log(base$Salary)
hist(base$Salary) # Depois da transformacao

train=1:200
#install.packages("gbm")
library(gbm)
# We run gbm() with the option distribution="gaussian" since this is a regression 
# problem; if it were a binary classification problem, we would use distribution="bernoulli".

set.seed(1)
boost.fit=gbm(Salary~.,data=base[train,],distribution="gaussian", 
              n.trees=1000)

summary(boost.fit)

yhat.boost=predict(boost.fit,newdata=base[-train,])
plot(yhat.boost,base[-train,"Salary"])
abline(0,1)

MSE=mean((yhat.boost-base[-train,"Salary"])^2)
sqrt(MSE) # As previsoes ficam em torno desse valor do real parametro

# Testando o parametro lambda

lambda=NA
Erro=NA

for(i in seq(from=0.001,to=0.02,by=0.001)){
  
  boost.fit=gbm(Salary~.,data=base[train,], distribution="gaussian", 
                   n.trees=1000, shrinkage=i, verbose=FALSE)
  
  yhat.boost=predict(boost.fit,newdata=base[-train,],n.trees=1000)
  
  lambda[i/0.001]=i
  Erro[i/0.001]=mean((yhat.boost-base[-train,"Salary"])^2)
  
}

plot(lambda,Erro)
abline(h=min(Erro),col="red") # Lambda = 0.018

# Abordagem de Bagging

set.seed(1)
bag.fit=randomForest(Salary~.,data=base,subset=train,
                     mtry=ncol(base)-1,importance=T)
bag.fit

yhat.bag=predict(bag.fit,newdata=base[-train,])
plot(yhat.bag,base[-train,"Salary"])
abline(0,1)

mean((yhat.bag-base[-train,"Salary"])^2) # Diminuiu o MSE para 0.23
min(Erro) # Lembre que no Boosting tinhamos 0.26
varImpPlot(bag.fit) # Number of times at bat during his career

# Exercicio 11

?Caravan
train=1:1000

set.seed(1)
boost.fit=gbm(Purchase~.,data=Caravan[train,],distribution="bernoulli", 
              n.trees=1000,shrinkage=0.01)

summary(boost.fit)
# Variaveis mais importante sao: PPERSAUT, MKOOPKLA, MOPLHOOG e MBERMIDD

boost.pred=predict.gbm(boost.fit,Caravan[-train,],type="response")
estimativa=rep("No",times=nrow(Caravan)-1000)
estimativa[boost.pred>=1.20]="Yes"

table(estimativa,Caravan[-train,"Purchase"])
mean(estimativa==Caravan[-train,"Purchase"])

table(Caravan[,"Purchase"])
348/5474 # Apenas 6.3% sao Yes, note que no modelo acertamos 20% das
# observacoes previstas como Yes, ou seja, melhor que o metodo naive