#EXO 1 

# 1.
x1 <- c(1, 2, 0, 3)
x2 <- c(0, 0, 2, 5)
x <- data.frame(x1,x2)
y <- c(1, 1,-1,-1)
mydata <- cbind(x,y)
dim(mydata)
mode(mydata$y)
# 2.
plot(mydata[c(1,2)],type="p",xlab="x1",ylab="x2",col=mydata$y+3,pch=mydata$y+2)
# 3.
library(e1071)
model = svm(as.factor(y) ~ x1+x2, data = mydata,
            type = "C", kernel="linear", scale = FALSE)
1
print(model)
# matrice des vecteurs de support
model$SV
# nombre de vecteurs de support
model$nSV
# 4.
cf <- coef(model)
abline(-cf[1]/cf[3],-cf[2]/cf[3], col = "green")
abline(-(cf[1]- 1)/cf[3],-cf[2]/cf[3], col = "blue")
abline(-(cf[1] + 1)/cf[3],-cf[2]/cf[3], col = "red")
points(model$SV, pch = 5, cex = 2)
# 5.
predict(model,subset(mydata, select =- c( y )) )
table(model$fitted, mydata$y)
# 6.
newdata <- data.frame( x1=c(2.5, 1, 0.5), x2=c(1, 3, 1.5) )
names(newdata)
# 7.
predict(model,newdata)
points(newdata[c(1,2)], pch = 16)


 # Ex 2. iris
 # 1. setosa
 data(iris)
 attach(iris)
 library(e1071)
 setosa <- as.factor(iris$Species == "setosa")
 model <- svm(setosa ~ Petal.Width + Petal.Length,
              data = iris, type = "C", kernel = "linear",
              scale = FALSE)
 plot(Petal.Length ~ Petal.Width, data = iris, col = setosa,
      main="Setosa")
 cf <- coef(model)
 abline(-cf[1]/cf[3],-cf[2]/cf[3], col = "green")
 abline(-(cf[1]- 1)/cf[3],-cf[2]/cf[3], col = "blue")
 abline(-(cf[1] + 1)/cf[3],-cf[2]/cf[3], col = "red")
 points(model$SV, pch = 5, cex = 2)
 table(model$fitted, iris$Species)
 # 2. versicolor
 iris2 <-subset(iris,subset=(iris$Species == "versicolor") |
                  (iris$Species == "virginica") )
 versicolor <- as.factor(iris2$Species == "versicolor")
 model2 <- svm(versicolor ~ Petal.Width + Petal.Length,
               data = iris2, type = "C", kernel = "linear",
               scale = FALSE)
 plot(Petal.Length ~ Petal.Width, data = iris2, col = versicolor,
      main="Versicolor")
 cf2 <- coef(model2)
 abline(-cf2[1]/cf2[3],-cf2[2]/cf2[3], col = "green")
 abline(-(cf2[1]- 1)/cf2[3],-cf2[2]/cf2[3], col = "blue")
 abline(-(cf2[1] + 1)/cf2[3],-cf2[2]/cf2[3], col = "red")
 points(model2$SV, pch = 5, cex = 2)
 table(model2$fitted, iris2$Species)
 # 3. multi-classes
 nrow(iris)
 3
 s <- seq(1,nrow(iris),2)
 iris_train <- iris[s,c("Petal.Width","Petal.Length","Species")]
 iris_test <- iris[-s,c("Petal.Width","Petal.Length","Species")]
 model3 <- svm(Species ~ Petal.Width + Petal.Length,
               data = iris_train, type = "C", kernel = "linear",
               scale = FALSE)
 plot(model3, iris_train)
 table(model3$fitted, iris_train$Species)
 prediction <- predict(model3,iris_test)
 table(prediction,iris_test$Species)







