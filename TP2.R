iris
#l'equivalent de shape dans python
dim(iris)

names(iris)
#donne comme un dataframe
str(iris)

iris$Petal.Length
iris$Species

#Levels <=> Select DISTINCT
levels(iris$Species)

# Equivalent à Describe python
summary(iris)
summary(iris$Species)

#Graphique
pie(table(iris$Species))
barplot(table(iris$Species))
#Découper les fenetre
par(mfrow=c(2,2))
par(mfcol=c(3,3))

collection1<-rep(c("setosa","versicolor","virginica"),c(15,19,12))
collection2<-rep(c("setosa","versicolor","virginica"),c(22,27,17))

# Ouvrir une nouvelle fenêtre graphique avec une taille spécifique
dev.new(width = 10, height = 10) 
par(mfrow=c(2,2))
pie(table(collection1))
pie(table(collection2))
barplot(table(collection1))
barplot(table(collection2))

min(iris$Petal.Length)
max(iris$Petal.Length)
mean(iris$Petal.Length)

sum(iris$Petal.Length)
length(iris$Petal.Length)
sum(iris$Petal.Length)/length(iris$Petal.Length)

sort(iris$Petal.Length)

ordLpetal <- sort(iris$Petal.Length)
ordLpetal # commenter le résultat
sum(ordLpetal)/length(ordLpetal)
ordLpetal[38]
(ordLpetal[75]+ordLpetal[76])/2
ordLpetal[113]

hist(iris$Petal.Length,col=grey(0.6),main="Histogramme")

#Exo 2
library(BioStatR)
head(Europe)
str(Europe)
summary(Europe)
range(Europe$Duree)
sd(Europe$Duree)
# Boîte à moustaches
boxplot(Europe$Duree,ylab="Durée (heures)")
points(1,mean(Europe$Duree),pch=2)

#EXO3
