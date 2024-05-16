#
# TD 2
#

# Ex 1

data(package="base")

# 3
iris
dim(iris)
names(iris)
str(iris)

# 4
iris$Petal.Length
iris$Species

# 5
levels(iris$Species)
summary(iris$Species)

# 6
table(iris$Species)

# 7
pie(table(iris$Species))
barplot(table(iris$Species))

# 8
collection1 <- rep( c("setosa","versicolor","virginica"), 
+c(15,19,12) )

collection1

collection2 <- rep(c("setosa","versicolor","virginica"),
c(22,27,17))

collection2

pie(table(collection1))
pie(table(collection2))
barplot(table(collection1))
barplot(table(collection2))

par(mfrow=c(2,2))
pie(table(collection1))
pie(table(collection2))
barplot(table(collection1))
barplot(table(collection2))

par(mfrow=c(2,1))
pie(table(collection1))
barplot(table(collection1))


#9
summary(iris$Petal.Length)

min(iris$Petal.Length)
max(iris$Petal.Length)
mean(iris$Petal.Length)

sum(iris$Petal.Length)
length(iris$Petal.Length)
sum(iris$Petal.Length)/length(iris$Petal.Length)

sort(iris$Petal.Length)
ordLpetal <- sort(iris$Petal.Length)

ordLpetal # commenter le resultat

sum(ordLpetal)/length(ordLpetal)

ordLpetal[38]

(ordLpetal[75]+ordLpetal[76])/2

ordLpetal[113]

#10
hist(iris$Petal.Length,col=grey(0.6),main="Histogramme")


# Exercice 2

library(BioStatR)

dim(Europe)
head(Europe)
str(Europe)
summary(Europe)
summary(Europe$Duree)


length(Europe$Duree)
min(Europe$Duree)
max(Europe$Duree)
m <- mean(Europe$Duree)
median(Europe$Duree)
quantile(Europe$Duree,0.25)
quantile(Europe$Duree,0.75)
quantile(Europe$Duree,0.50)

range(Europe$Duree)

variance.empirique <- sum( (Europe$Duree - m)^2 )/length(Europe$Duree)
variance.empirique

varriance.corrigee <- variance.empirique * length(Europe$Duree)/
(length(Europe$Duree)-1)

varriance.corrigee

var(Europe$Duree)

sd(Europe$Duree)

sqrt(var(Europe$Duree))

# Boite a moustaches
boxplot(Europe$Duree,ylab="Duree (heures)")
points(1,mean(Europe$Duree),pch=2)


# Exercice 3

# 1
Femmes<-c(105,110,112,112,118,119,120,120,125,126,127,128,130,132,133,134,135,
138,138,138,138,142,145,148,148,150,151,154,154,158)
Femmes
Hommes<-c(141,144,146,148,149,150,150,151,153,153,153,154,155,156,156,160,160,
160,163,164,164,165,166,168,168,170,172,172,176,179)
Hommes

histo.fem <- hist(Femmes,breaks=c(104,114,124,134,144,154,164,174,184))
Femmes[Femmes >104 & Femmes <=114]

effectif.fem <- histo.fem$counts
effectif.fem
sum(effectif.fem)

frequence.fem<-effectif.fem/sum(effectif.fem)
frequence.fem
print(frequence.fem,digits=3)

histo.hom<-hist(Hommes,breaks=c(104,114,124,134,144,154,164,174,184))

effectif.hom<-histo.hom$counts
effectif.hom
sum(effectif.hom)

frequence.hom<-effectif.hom/sum(effectif.hom)
print(frequence.hom,digits=3)

# 2
histo.fem<-hist(Femmes,breaks=c(104,114,124,134,144,154,164,174,184))
histo.hom<-hist(Hommes,breaks=c(104,114,124,134,144,154,164,174,184))

# 3
Ensemble<-c(Femmes,Hommes)
Ensemble

mean(Ensemble)
mean(Femmes)
mean(Hommes)


# 4
histo.ens<- hist(Ensemble,breaks=c(104,114,124,134,144,154,164,174,184))
histo.ens$counts
histo.ens$mids

mean.ensemble <- sum(histo.ens$counts * histo.ens$mids)/length(Ensemble)
mean.ensemble
mean.fem <- sum(histo.fem$counts*histo.fem$mids)/length(Femmes)
mean.fem
mean.hom <- sum(histo.hom$counts*histo.hom$mids)/length(Hommes)
mean.hom

# 5
median(Ensemble)
median(Femmes)
median(Hommes)

quantile(Ensemble,0.50,type=6)
quantile(Femmes,0.50,type=6)
quantile(Hommes,0.50,type=6)

# 6
quantile(Ensemble,0.25,type=6)
quantile(Femmes,0.25,type=6)
quantile(Hommes,0.25,type=6)

quantile(Ensemble,0.75,type=6)
quantile(Femmes,0.75,type=6)
quantile(Hommes,0.75,type=6)

IQR(Ensemble,type=6)
#quantile(Ensemble,0.75,type=6)-quantile(Ensemble,0.25,type=6)
IQR(Femmes,type=6)
IQR(Hommes,type=6)

# 7
var(Ensemble)
var(Femmes)
var(Hommes)

varemp.ens <- sum( (Ensemble-mean(Ensemble))^2 )/length(Ensemble)
varemp.ens

varcor.ens <- varemp.ens*length(Ensemble)/(length(Ensemble) -1)
varcor.ens

sd(Ensemble)
sd(Femmes)
sd(Hommes)

# 8
variance.emp.ensemble <- sum(histo.ens$counts*(histo.ens$mids-mean.ensemble)^2)/length(Ensemble)
variance.emp.ensemble

variance.emp.fem <- sum(histo.fem$counts*(histo.fem$mids-mean.fem)^2)/length(Femmes)
variance.emp.fem

variance.emp.hom <- sum(histo.hom$counts*(histo.hom$mids-mean.hom)^2)/length(Hommes)
variance.emp.hom

sqrt(variance.emp.ensemble)
sqrt(variance.emp.fem)
sqrt(variance.emp.hom)






