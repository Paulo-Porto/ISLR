
# Bases de Dados no Link - http://faculty.marshall.usc.edu/gareth-james/ISL/data.html


# Exercicio 08

# a)

college=read.csv("C:/Users/M100396/Desktop/ISL in R/Capítulo 01/College.csv",header=T)

# b)

rownames(college)=college[,1]
fix(college)

college=college[,-1]
fix(college)

# c) 

summary(college)

A = na.omit(college[,1:10])
dim(A)

plot(A)

names(college)
A = na.omit(college[,c(1,9)])
A[,1]=as.factor(A[,1])
plot(A,type="boxplot")

Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)
fix(college)

summary(college) # 78 Universidades de Elite

boxplot(college$Outstate~college$Elite)

par(mfrow=c(2,2))

edit(college)
ncol(college)


for(j in 2:(ncol(college)-1)){
 
  for(i in 1:4){
    hist(college[,j],breaks=20*i,main=names(college)[j])
  }
   
}

# Exercicio 09

auto=read.csv("C:/Users/M100396/Desktop/ISL in R/Capítulo 01/Auto.csv",header=T,na.strings="?")
auto=na.omit(auto)
edit(auto)

summary(auto)

# b)

range(auto[,1])
nomes=names(auto)
nomes=nomes[1:7]

valores=matrix(NA,nrow=7,ncol=2)

for(i in 1:7){
  valores[i,]=range(auto[,i])
}

limites=data.frame(nomes,valores)
limites
names(limites)=c("Variável","Lim Inf","Lim Sup")

# c)

medias=apply(auto[,1:7],2,mean)
desvios=apply(auto[,1:7],2,sd)

# d)

n_medias=apply(auto[c(1:9,86:nrow(auto)),1:7],2,mean)
n_desvios=apply(auto[c(1:9,86:nrow(auto)),1:7],2,sd)

limites=data.frame(limites,medias,n_medias,desvios,n_desvios)
limites

# e)

plot(auto[,-9]) # vamos usar esses plots para descrever

# Na primeira linha, a medida que o número de cylinders aumenta a mpg parece diminuir
# Isso também pode ser visto para as variáveis displacement, horsepower, weight
# Já as variáveis year e origin parecem ter uma relação positiva, enquanto acceleration parece não ter relacao

cor(auto[,-9])[-1,1] # As correlacoes parecem corroborar as impressoes visuais

# A análise da segunda linha aponta para uma relacao positiva entre o numero
# de cylinders e as variaveis displacement, horsepower e weignt
# ja a aceleracao parece ter uma relacao negativa e as outras variaveis pouca correlacao

cor(auto[,-9])[-c(1,2),2]

# A relacao entre todas as variaveis pode ser vista na matrix completa de correlacao

cor(auto[,-9])

# f)

# Vimos as correlacoes com mpg anteriormente
cor(auto[,-9])[-1,1]

# Elas parecem apontar que as quatro primeiras variaveis, a saber, cylinders
# displacemente, horsepower e weight seriam bons preditores

# Exercicio 10

library("MASS")

edit(Boston)
?Boston # A base contem 506 linhas e 14 colunas
summary(Boston)

# b)

pairs(Boston[,-4])
cor(Boston[,-4]) # As relacoes podem ser vistas na matrix de correlacao

# c)

# Per capta crime rate esta descrito pela variavel crim
# Vamos olhar para os graficos e correlacoes

par(mfrow=c(3,4))

for(i in 1:2){
  plot(Boston$crim,Boston[,i+1],ylab=names(Boston)[i+1])
}

for(i in 4:13){
  plot(Boston$crim,Boston[,i+1],ylab=names(Boston)[i+1])
}

cor(Boston[,-4])[1,-1]
# As relacoes aqui parecem nao ser tao fortes do ponto de vista da correlacao linear

# d)

par(mfrow=c(1,1))
plot(Boston$crim)
Boston[Boston$crim==max(Boston$crim),] # Distrito 381 dessa base de dados

# e)

sum(Boston$chas) # 35 suburbios

# f) 

median(Boston$ptratio) # 19.05

# g)

plot(Boston$medv)
Boston[Boston$medv==min(Boston$medv),] # Distritos 399 e 406, com medv = 5

matriz=matrix(NA,nrow=14,ncol=3)
matriz[,1]=names(Boston)

for(i in 1:14){
  matriz[i,2:3]=range(Boston[,i])
}

matriz=data.frame(matriz,t(Boston[Boston$medv==min(Boston$medv),]))
matriz

# h)

mais_7=t(Boston[Boston$rm>=7,])
ncol(mais_7) #64 suburbios

mais_8=t(Boston[Boston$rm>=8,])
ncol(mais_8) #13 suburbios

# Vamos compara-los ao range de cada variavel como fizemos anteriormente

matriz=matrix(NA,nrow=14,ncol=3)
matriz[,1]=names(Boston)

for(i in 1:14){
  matriz[i,2:3]=range(Boston[,i])
}

media=apply(mais_8, 1, mean)
matriz=data.frame(matriz,media)
matriz[,4]=round(matriz[,4],digits=2)
matriz