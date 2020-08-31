
# LAB: Linear Regression

library(MASS)
'install.packages("ISLR")
'
library(ISLR)

# 3.6.2 Simple Linear Regression

edit(Boston)
names(Boston)
?Boston

attach(Boston)

lm.fit=lm(medv~lstat,data=Boston)
lm.fit

plot(medv,lstat)
abline(lm.fit[1],col="red",lwd=2)

summary(lm.fit)

confint(lm.fit) # Intervalos de Confiança

par(mfrow =c(2,2))
plot(lm.fit) # Gráficos de Diagnostico
par(mfrow =c(1,1))

plot(predict(lm.fit),residuals(lm.fit)) # Geracao manual dos Gráficos Diagnostico
plot(predict(lm.fit),rstudent(lm.fit)) # Geracao manual dos Gráficos Diagnostico

plot(hatvalues(lm.fit)) # Valores alavancados, enquanto os outliers sao observacoes
# onde a variavel resposta Y tem valores extremos, os pontos de alavancagem sao
# aqueles onde o preditor X tem valores extremos

# Regressao Multipla

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

?summary.lm

coef(lm.fit)

# VIF - Variance Inflation Factor para Identificar/Medir Colinearidade

'install.packages("car")
'

library(car)
vif(lm.fit)

# Interacao de termos

# lstat:black -> interaction term between lstat and black

# lstat*age simultaneously includes lstat, age,and the interaction term lstat×age
# it is a shorthand for lstat+age+lstat:age

?Boston

summary(lm(medv~lstat*age))

# Tranformacoes nao lineares

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

# The anova() function performs a hypothesis test comparing the two models. 
# The null hypothesis is that the two models fit the data equally well, and the
# alternative hypothesis is that the full model is superior

par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(1,1))

# Modelos Polinomiais de Maior Ordem

lm.fit5=lm(medv~poly(lstat,degree=5))
summary(lm.fit5)

plot(medv,log(rm))
lm.fit.log=lm(medv~log(rm))
summary(lm.fit.log)

# Preditores Qualitativos

?Carseats
edit(Carseats)
summary(Carseats)
attach(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price :Age,data=Carseats)
summary(lm.fit)

contrasts(ShelveLoc) # Importante para entender o output da regressao

# Exercicios Aplicados

attach(Auto)
edit(Auto)

lm.fit=lm(mpg~horsepower)
summary(lm.fit)

# Existe uma relacao negativa entre a variavel resposta e o preditor

lm.fit$coefficients%*%c(1,98) # Estimativa Pontual de 24,46 mpg

confint(lm.fit)[,1]%*%c(1,98) # Limit Inf de 21,81 mpg
confint(lm.fit)[,2]%*%c(1,98) # Limit Sup de 27,12 mpg

alvo=which(horsepower==98)
alvo # Observacoes com horsepower igual a 98

predict.lm(lm.fit,interval="prediction")[alvo,]
# Estimativa pontual de 24,46 e Intervalor entre 14,8 e 34,1

plot(horsepower,mpg)
abline(lm.fit[1],col="red")

plot(predict(lm.fit),residuals(lm.fit)) # Residuos com um padrao quadratico
plot(hatvalues(lm.fit)) # Alguns valores alavancados de horsepower, vide hist
hist(horsepower)

# Questao 09

attach(Auto)
plot(Auto)

round(cor(Auto[,-ncol(Auto)]),digits = 1)

lm.fit=lm(mpg~.-name,data=Auto)

summary(lm.fit)

# i) Sim, existe relacao, mas pra mim foi surpreendente o fato de variaveis
# com menor correlacao como acceleration serem significativas, equanto outras
# com maior correlacao com mpg como cylinders nao sao

# ii) displacement, weight, year e origin

# iii) Em cada ano carros sao produzidos com mais 0,75 mpg que no ano anterior

# d)

par(mfrow =c(2,2))
plot(lm.fit) # Gráficos de Diagnostico
par(mfrow =c(1,1))

# O grafico de residuos mostra ainda uma leve tendencia quadratica e alguns outliers

# Essas observacoes sao as mesmas que possuem alto valor de alavancagem no grafico de
# escala e localizacao

# Questao 10

edit(Carseats)
attach(Carseats)

lm.fit=lm(Sales ~ Price + Urban + US,data=Carseats)
summary(lm.fit)

# As vendas sao negativamente correlacionadas com o preco, o que faz sentido
# porque naturalmente os itens mais baratos vendem mais

# O efeito da variavel Urban nao e signficativo, mas os Carseats dos EUA tem um
# nivel de venda maior do que os que vem fora dos EUA

lm.fit=lm(Sales ~ .,data=Carseats)
summary(lm.fit)

# Podemos rejeitar H0 para: CompPrice, Income, Advertising, Price
# ShelveLoc, Age

lm.fit=lm(Sales ~ CompPrice+Income+Advertising+Price+ShelveLoc+Age,data=Carseats)
summary(lm.fit)

# Ambos os modelos tem a mesma aderencia aos dados (R2 Ajustado)

confint(lm.fit)

par(mfrow =c(2,2))
plot(lm.fit) # Nao parecem haver grandes problemas
par(mfrow =c(1,1))

# Questao 11

set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

plot(x,y)

lm.y=lm(y~x+0)
summary(lm.y)

lm.x=lm(x~y+0)
summary(lm.x)

# Questao 12

noise=0.25

set.seed(1)
x=rnorm(100)
eps=rnorm(100,sd=sqrt(noise))

y=-1+x/2+eps # Intercepto = -1 e Beta = 0.50

par(mfrow=c(1,3))
plot(x,x)    
plot(x,eps)
plot(x,y)
par(mfrow=c(1,1))

lm.fit=lm(y~x)
summary(lm.fit) # Estimativas identicas ao teórico
confint(lm.fit)

plot(x,y)
abline(lm.fit[1],col="red")
abline(a=-1,b=0.5,col="blue")

lm.fit2=lm(y~poly(x,degree=2))
summary(lm.fit2) # A regressao polinomial nao melhora o modelo

# Questao 14

set.seed(1)
x1=runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y= 2 + 2*x1 + 0.3*x2 + rnorm(100) # Intercepto = 2, Beta1 = 2, Beta2 = 0.3

par(mfrow=c(3,1))
plot(x1,x1)
plot(x2,x1)
plot(y,x1)
par(mfrow=c(1,1))

dados=data.frame(x1,x2,y)
cor(dados) # Correlacao alta entre X1 e X2 --> Colineariedade!

lm.fit=lm(y~.,data=dados)
summary(lm.fit) # Intercepto = 2.1, Beta1 = 1.4, Beta2 = 1.0

# Nao podemos rejeitar a hipotese de que Beta2 seja zero!

lm.fit=lm(y~x1,data=dados)
summary(lm.fit) # Intercepto = 2.1, Beta1 (X1) = 2.0 com significancia
plot(x1,y)
abline(lm.fit[1],col="red")

lm.fit=lm(y~x2,data=dados)
summary(lm.fit) # Intercepto = 2.4, Beta1 (X2) = 2.9 com significancia
plot(x2,y)
abline(lm.fit[1],col="red")

# Em certo sentido o resultado acima contradiz o da regressao multipla, mas pelo
# motivo da regressao multipla demonstrar que, ao considerar X1 a informacao
# trazida por X2 (que e funcao de X1) se torna negligenciavel

# Inclusao de uma nova observacao muda sensivelmente as conclusoes!

x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y,6)
dados=data.frame(x1,x2,y)

lm.fit=lm(y~.,data=dados)
summary(lm.fit) # Agora X2 tem significancia, mas nao X1
plot(lm.fit) # A Nova observacao e high leverage em X2

lm.fit=lm(y~x1,data=dados)
summary(lm.fit) # Intercepto = 2.3, Beta1 (X1) = 1.8 com significancia

lm.fit=lm(y~x2,data=dados)
summary(lm.fit) # Intercepto = 2.3, Beta1 (X2) = 3.1 com significancia

# Questao 15

?Boston
attach(Boston)

lm.fit=lm(crim ~ . , data=Boston)
summary(lm.fit) # Resposta da letra B

Betas=matrix(NA,nrow=ncol(Boston)-1,ncol=2,dimnames=list(names(Boston)[-1],c("Betas.Univ","Betas.Mult")))
Auxiliar=NA

for(i in 2:ncol(Boston)){
 
  Auxiliar=lm(crim~Boston[,i])
  Betas[i-1,1]=Auxiliar$coefficients[2]
  Betas[i-1,2]=lm.fit$coefficients[i]
   
}

Betas
?plot
plot(Betas[,1],Betas[,2],xlab="Betas.Univ",ylab="Betas.Mult") # Variavel nox e outlier

intervalo=c(-5,5)

plot(Betas[-4,1],Betas[-4,2],xlab="Betas.Univ",ylab="Betas.Mult",xlim=intervalo,ylim=intervalo) # Removendo nox
abline(0,1) # Resposta letra C

names(Boston)

plot(zn,crim)
lm.fit=lm(crim~poly(zn,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(indus,crim)
lm.fit=lm(crim~poly(indus,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

chas=as.numeric(chas)
plot(chas,crim)
lm.fit=lm(crim~poly(chas,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(nox,crim)
lm.fit=lm(crim~poly(nox,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(rm,crim)
lm.fit=lm(crim~poly(rm,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(age,crim)
lm.fit=lm(crim~poly(age,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(dis,crim)
lm.fit=lm(crim~poly(dis,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(rad,crim)
lm.fit=lm(crim~poly(rad,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(tax,crim)
lm.fit=lm(crim~poly(tax,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(ptratio,crim)
lm.fit=lm(crim~poly(ptratio,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(black,crim)
lm.fit=lm(crim~poly(black,degree = 3))
summary(lm.fit) # Sem indicio de relacao quadratica

plot(lstat,crim)
lm.fit=lm(crim~poly(lstat,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica

plot(medv,crim)
lm.fit=lm(crim~poly(medv,degree = 3))
summary(lm.fit) # Algum indicio de relacao quadratica
