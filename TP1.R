suite = 1:12
seq(from = 1, to = 10, by = 1 )
seq(1,12,2)

serie1<-c(1.2,36,5.33,-26.5)
serie2<-c("bleu","vert","marron") 
serie3<-c(T,T,F,F,T)
serie4<-c(1.2,36,NA,-26.5)
#le type :
mode(serie1)
#la longueur
length(serie1)

serie1[3:4]

#Cancatener
x<-c(2.3,3.5,6,14,12)
y<-c(3.2,5,0.7,1,3.5)
z<-c(x,y)

#Extraire des données d’un vecteur ou de deux vecteurs
x[c(2,5)]
#L’utilisation du signe tiret permet de supprimer des composantes
#supprimer les 2ème et 3ème composantes du vecteur x
x[-c(2,3)]

x[x>4]
y[x>4]

#remplacer la valeur 1 par la valeur 25
x[x==1]<-25
x[x>=5]<-20

#Répéter les données d’un vecteur
rep(x=1,times=50)

#Nommer les composantes d’un vecteur
notes.Jean = c(Anglais = 12, Informatique = 19.5, Biologie = 14)
#Seconde façon
matiere<-c("Anglais","Informatique","Biologie")
matiere
note <- c(12,19.5,14)
note
names(note)<-matiere
note

#pour supprimer les noms
names(note) <- NULL
#Trier les composantes d’un vecteur
sort(note)
#l’ordre décroissant
rev(sort(note))
sort(note,T)
#la négation
!logic1
# la conjonction ET
logic1 & logic2
#logic1[1] & logic2[1] est identique
logic1 && logic2
#la disjonction OU
logic1 | logic2
#logic1[1] | logic2[1]
logic1 || logic2
#connaître le répertoire de travail
getwd()
#changer le répertoire de travail
setwd("C:\\Data")

#LIRE DES DONNEES
tab = read.table("table1.txt")
View(tab)

#Demander une colonne de cet objet
tab$V1
#l’élément de la première ligne et de la première colonne
tab[1,c(1)]
tab[1,1]
#deux premières lignes et de la première colonne
tab[1:2,1]
tab[1:2,1:2]

V1<-tab$V1
V2<-tab$V2
#Si on a des headers dans notre fichier
read.table("table2.txt",header=T)
#Si les décimales sont notés par virgule 
read.table("table3.txt",dec=",")
#Si les col sont séparées par ;
read.table("table4.txt",sep=";")
#ouvrir un fichier de données sans avoir à indiquer son emplacement
read.table(file.choose())
read.csv(file.choose())
read.csv2(file.choose())

#EX01
x=c(101:112)
x2=rep(c(4,6,3),4)
x3 = c(rep(4,8),rep(6,7),rep(3,5))

#EXO2
poids = c(28, 27.5, 27, 28, 30.5, 30, 31, 29.5, 30, 31, 31, 31.5,32, 30, 30.5)
poids1 = c(40, 39, 41, 37.5, 43)
nouveau.poids = c(rep(poids1, 2), poids[5:15])

#EXO3
nom=c("a","b","c", "d", "e", "f","g", "h","i","j")
age=seq(20,59,4)
names(age)=nom

poids=seq(50,99,5)
taille=seq(50,99,5)

poids.lourds = poids[poids>80]
taille.poids.lourds =taille[poids>80]
taille.vieux.poids.lourds = taille[poids>80 & age > 30 ]