# Ex 1

library(BioStatR)

class(Mesures)
dim(Mesures)
str(Mesures)
levels(Mesures$espece)

# 1.
vec <- (Mesures$espece=="glycine blanche")
glycine.blanche <- subset(Mesures,subset=vec)

class(glycine.blanche)
dim(glycine.blanche)
nrow(glycine.blanche)
ncol(glycine.blanche)
str(glycine.blanche)

# 2.
(n<- length(glycine.blanche$taille))
(moyenne<-mean(glycine.blanche$taille))
(quantile<-qt(0.975,n-1))

# ecart-type corrig?
(ecart.type<-sd(glycine.blanche$taille))

moyenne-quantile*(ecart.type/sqrt(n))
moyenne+quantile*(ecart.type/sqrt(n))

# Ex 2
(variance<- var(glycine.blanche$taille))
alpha=0.05
(l1<- qchisq(alpha/2,n-1)) 
(l2<- qchisq(1- alpha/2, n-1)) 

(n-1)*variance/l2 # 7.712453
(n-1)*variance/l1 # 16.63339

# Ex 3.
# 1. 
v <- c(19.6, 20, 20.2, 20.1, 20, 19.9, 20, 20.3, 20.1, 19.8)
(n<- length(v))
(moyenne<-mean(v))

# ecart-type corrig?
(ecart.type<-sd(v))
alpha <- 0.05
(quantile<-qt(1-alpha/2,n-1))

moyenne-quantile*(ecart.type/sqrt(n))
moyenne+quantile*(ecart.type/sqrt(n))

# 2. 
(sigma <- 0.2)
(quantile <- qnorm(0.975))
moyenne-quantile*(sigma/sqrt(n))
moyenne+quantile*(sigma/sqrt(n))

# Ex 4.
# 1.
vec <- rnorm(20,mean=12,sd=2)
# 2.
alpha <- 0.01
n <- 20
(k1<- qchisq(alpha/2,n)) 
(k2<- qchisq(1- alpha/2, n)) 

mu <- 12
variance <- sum( (vec-mu)^2 )/n
  
n*variance/k2 
n*variance/k1 
