
# Lab

library(ISLR)

edit(Smarket)
plot(Smarket)

round(cor(Smarket[,c(-1,-9)]),digits=2)

attach(Smarket)

plot(Volume)
boxplot(Volume~Year)

log.fit=glm(Direction~.-Today-Year,data=Smarket,family=binomial)
summary(log.fit)
round(coef(log.fit),digits=2)

log.probs=predict(log.fit,type="response") # P(Direction=1/X=x)
log.probs[1:10] # Probabilidade de subir dadas as observações

contrasts(Direction)

log.pred=rep("Down",length(log.probs))
log.pred[log.probs>0.5]="Up"
log.pred

confusion=table(log.pred,Direction) # Confusion Table
sum(diag(confusion))/sum(confusion) # % Acerto

# Agora vamos repetir o processo deixando parte dos dados de fora
# assim conseguiremos melhor estimar a taxa de acerto/erro das previsoes

train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

length(Direction.2005)

# Rodar novamente o processo

log.fit=glm(Direction~.-Today-Year,data=Smarket,family=binomial,
            subset=train) # Treinando sem o Ano 2005

log.probs=predict(log.fit,Smarket.2005,
                  type="response") # Usando para prever 2005

log.pred=rep("Down",length(log.probs))
log.pred[log.probs>0.5]="Up"
log.pred

confusion=table(log.pred,Direction.2005) # Confusion Table
confusion
sum(diag(confusion))/sum(confusion) # % Acerto

mean(log.pred==Direction.2005) # % Acerto
mean(log.pred!=Direction.2005) # % Erro

# Modelo simplificado

log.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,
            subset=train) # Treinando sem o Ano 2005

log.probs=predict(log.fit,Smarket.2005,
                  type="response") # Usando para prever 2005

log.pred=rep("Down",length(log.probs))
log.pred[log.probs>0.5]="Up"

mean(log.pred==Direction.2005) # % Acerto
mean(log.pred!=Direction.2005) # % Erro

table(log.pred,Direction.2005) # Note que o algoritmo acerta bastante quando
# a previsão e de que o mercado vai subir

# Previsão para valores pontuais

# Depois de ter rodado o modelo, basta fazer:

predict(log.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# Linear Discriminant Analysis

attach(Smarket)
train=(Year<2005)
summary(Smarket)

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit,Smarket.2005)
lda.class=lda.pred$class

table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

# Entendendo a previsao do LDA

lda.pred$posterior[,2] # Probabilidade de UP

sum(lda.pred$posterior[,2]<0.5) # Equivalente ao livro
sum(lda.pred$posterior[,2]>=0.5) # Equivalente ao livro

# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class

table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

# K-Nearest Neighbors

library(class)

attach(Smarket)
train=(Year<2005)

train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

knn.acerto=NA

for(i in seq(from=0,to=10,by=2)[2:6]){

  set.seed(1)
  knn.pred=knn(train.X,test.X,train.Direction,i)
  table(knn.pred,Direction.2005)
  knn.acerto[i/2]=mean(knn.pred==Direction.2005)
  
}

plot(knn.acerto,type="l")

edit(Caravan)
attach(Caravan)

standart.Caravan=scale(Caravan[,-86])
dim(standart.Caravan)

corte=1000
test=1:corte
train.X=standart.Caravan[-test,]
test.X=standart.Caravan[test,]

train.Y=Purchase[-test]
test.Y=Purchase[test]

set.seed(1)

knn.pred=knn(train.X,test.X,train.Y,k=1)
table(knn.pred,test.Y)
table(knn.pred,test.Y)[2,2]/sum(table(knn.pred,test.Y)[2,])

alcance=6

matriz=matrix(NA,nrow=alcance,ncol=alcance)

for(i in 1:alcance){
  
  test=1:(corte+500*i)
  train.X=standart.Caravan[-test,]
  test.X=standart.Caravan[test,]
  train.Y=Purchase[-test]
  test.Y=Purchase[test]
  
  for(j in 1:alcance) {
    
    knn.pred=knn(train.X,test.X,train.Y,j)
    matriz[i,j]=round(table(knn.pred,test.Y)[2,2]/sum(table(knn.pred,test.Y)[2,]),digits = 2)
    
  }
  
}

matriz
max(matriz)

corte=3000
test=1:corte
train.X=standart.Caravan[-test,]
test.X=standart.Caravan[test,]

train.Y=Purchase[-test]
test.Y=Purchase[test]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
table(knn.pred,test.Y)[2,2]/sum(table(knn.pred,test.Y)[2,])

# Modelo Logistico

corte=1000
test=1:corte
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)

glm.probs=predict(glm.fit,Caravan[test,],type = "response")
glm.pred=rep("No",times=corte)
glm.pred[glm.probs>0.5]="Yes"

table(glm.pred,Purchase[test])

# Teste de Resultados

alcance=6

matriz=matrix(NA,nrow=alcance,ncol=alcance,dimnames=list(seq(from=1000,to=3500,by=500),round(seq(from=1/12,to=6/12,by=1/12),digits=2)))

for(i in 1:alcance){
  
  test=1:(corte+500*(i-1))
  glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
  glm.probs=predict(glm.fit,Caravan[test,],type="response")
  
  
  for(j in 1:alcance) {
    
    glm.pred=rep("No",times=corte)
    glm.pred[glm.probs>j/12]="Yes"
    
    table(glm.pred,Purchase[test])
    
    matriz[i,j]=round(table(glm.pred,Purchase[test])[2,2]/sum(table(glm.pred,Purchase[test])[2,]),digits = 2)
    
  }
  
}

matriz
max(matriz)

# Applied - Questao 10

library(ISLR)
edit(Weekly)
?Weekly

summary(Weekly[,c(-1,-ncol(Weekly))])
plot(Weekly[,c(-1,-ncol(Weekly))]) # Nao parece haver padroes claros

'SP500=NA

for(i in 2:length(Today)){
  SP500[1]=Today[1]/100
  SP500[i]=(1+Today[i]/100)*(1+SP500[i-1])-1
}

plot(SP500) # Retorno acumulado do indice SP500'

attach(Weekly)
names(Weekly)

log.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)
summary(log.fit) # No Lag2 parece haver alguma informacao relevante

log.probs=predict(log.fit,Weekly,type="response")

log.pred=rep("Down",length(log.probs))
log.pred[log.probs>0.5]="Up"
confusion=table(log.pred,Direction)

mean(log.pred==Direction) # 56% de acerto, mas e superestimado

# Tambem e possivel ver que o algoritmo acerta mais do que 50% nos dois
# casos, ou seja, quando preve Up ou quando preve Down

# Previsao com Regressao Logistica

train=(Year<=2008)
test=Weekly[!train,]

log.fit=glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)
summary(log.fit) # No Lag2 parece haver alguma informacao relevante

log.probs=predict(log.fit,test,type="response")

log.pred=rep("Down",length(log.probs))
log.pred[log.probs>0.5]="Up"
confusion=table(log.pred,test$Direction)
confusion

mean(log.pred==test$Direction) # 62% de acerto! Impressionante

# Previsao com Linear Discriminant Analysis

library(MASS)
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit

lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class

table(lda.class,test$Direction)
mean(lda.class==test$Direction) # Mesmo resultado da Logistica!!

# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit

qda.class=predict(qda.fit,test)$class

table(qda.class,test$Direction) #Modelo nao previu quedas!
mean(qda.class==test$Direction) # Acertou 59% das previsoes de alta

# Parece indicar que a regra de decisão dos dados nao e quadratica

# Metodo KNN

train.X=data.frame(Lag2[train])
test.X=data.frame(Lag2[!train])
train.Direction=Direction[train]

knn.pred=knn(train.X,test.X,train.Direction,k=1)

table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train]) # Nao teve bom resultado, pouco acima 50%

# Problema 11

Auto
attach(Auto)

mpg01=mpg
mpg01[mpg01<median(mpg)]=0
mpg01[mpg01>median(mpg)]=1
mpg01

dados=data.frame(mpg01,Auto)
dados

plot(dados) # A parte descritiva das relacoes pode ser feita a partir
# da analise desse grafico e sera omitida, pois ja foi realizada em
# exercicio de capitulos anteriores

# Variaveis mais associadas

# Sugerir um metodo para escolha, vamos ver quais variaveis sao mais
# associadas com mpg01 num modelo de regressao linear

attach(dados)
names(dados)
lm.fit=lm(mpg01~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=dados[,-10])
summary(lm.fit) # Vamos usar cylinders,horsepower,weight e year

dim(dados)[1]
corte=1:(0.75*dim(dados)[1])

train=dados[corte,c(-2,-4,-7,-9,-10)]
test=dados[-corte,c(-2,-4,-7,-9,-10)]

plot(train)

# Modelo LDA

library(MASS)

# Train Error - LDA

lda.fit=lda(mpg01~.,data=train)
lda.fit

lda.pred=predict(lda.fit,train)
lda.class=lda.pred$class

table(lda.class,train$mpg01) 
mean(lda.class==train$mpg01) # 92% de acerto na base de treinamento

# Test Error - LDA

lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class

table(lda.class,test$mpg01) # Acertou todas as previsões acima da media!
mean(lda.class==test$mpg01) # 84% de acerto na base teste

# Modelo QDA

# Train Error - QDA

qda.fit=qda(mpg01~.,data=train)
lda.fit

qda.pred=predict(qda.fit,train)
qda.class=qda.pred$class

table(qda.class,train$mpg01) 
mean(qda.class==train$mpg01) # 93% de acerto na base de treinamento

# Test Error - QDA

qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class

table(qda.class,test$mpg01) # Acertou todas as previsões acima da media!
mean(qda.class==test$mpg01) # 80% de acerto na base teste

# Regressao Logistica

log.fit=glm(mpg01~.,data=train,family=binomial)
summary(log.fit) # No Lag2 parece haver alguma informacao relevante

log.probs=predict(log.fit,test,type="response")

log.pred=rep(0,length(log.probs))
log.pred[log.probs>0.5]=1
table(log.pred,test$mpg01) # Acertou todas as previsões acima da media!
mean(log.pred==test$mpg01) # 80% de acerto na base teste

# Metodo KNN

library(class)

train.X=data.frame(train[,-1])
test.X=data.frame(test[,-1])
train.Direction=train[,1]
test.Direction=test[,1]

knn.pred=knn(train.X,test.X,train.Direction,k=1)

table(knn.pred,test.Direction)
mean(knn.pred==test.Direction)

acertos=NA

for(i in 1:20){
 
  knn.pred=knn(train.X,test.X,train.Direction,k=i)
  acertos[i]=mean(knn.pred==test.Direction)
  
}

plot(acertos)
plot(acertos,xlim=c(10,20)) # maximo no K=16, modelo muito complexo

knn.pred=knn(train.X,test.X,train.Direction,k=16)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction) # 77% de acertos

# Exercicio 13 - Prever a Criminalidade

?Boston
edit(Boston)
attach(Boston)

crime=crim
crime[crime>median(crim)]="Acima"
crime[crime<median(crim)]="Abaixo"

dados=data.frame(crime,Boston[,-1]) # Retirando o valor da criminalidade
dados[,1]=as.factor(dados[,1])
dim(dados)

set.seed(1)
vetor=sample(1:dim(dados)[1],size=0.15*dim(dados)[1],replace=FALSE)
train=dados[-vetor,]
test=dados[vetor,]

# Modelo Logistico

detach(Boston)
attach(train)

crime=as.factor(crime)
contrasts(crime)

log.fit=glm(crime~.,data=train,family=binomial)
summary(log.fit) #Significativos zn+nox+dis+rad+tax+ptratio+black+medv

log.fit=glm(crime~zn+nox+dis+rad+tax+ptratio+black+medv,data=train,family=binomial)
log.probs=predict(log.fit,test,type="response")

log.pred=rep("Abaixo",length(log.probs))
log.pred[log.probs>0.5]="Acima"
table(log.pred,test$crime)
mean(log.pred==test$crime) # 90% de acerto

# Modelo LDA

lda.fit=lda(crime~zn+nox+dis+rad+tax+ptratio+black+medv,data=train)
lda.fit

lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class

table(lda.class,test$crime) 
mean(lda.class==test$crime) # 87% de acerto

# Modelo QDA

qda.fit=qda(crime~zn+nox+dis+rad+tax+ptratio+black+medv,data=train)
qda.fit

qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class

table(qda.class,test$crime) 
mean(qda.class==test$crime) # 87% de acerto

# Metodo KNN

library(class)

train.X=data.frame(train[,-1])
test.X=data.frame(test[,-1])
train.Direction=train[,1]
test.Direction=test[,1]

knn.pred=knn(train.X,test.X,train.Direction,k=1)

table(knn.pred,test.Direction)
mean(knn.pred==test.Direction) # 92% de acertos

acertos=NA

for(i in 1:20){
  
  knn.pred=knn(train.X,test.X,train.Direction,k=i)
  acertos[i]=mean(knn.pred==test.Direction)
  
}

plot(acertos) # k = 3 e o melhor modelo

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction) # 96% de acertos