---
  title: "Topik 1. Pengenalan Program R"
author: Sahda Huwaidah Estiningtyas
date: "`21 Februari 2024`"
---
  

x<-rnorm(n=100, mean=0, sd=1)
y<-rnorm(n=100, mean=3, sd=0.5)

x
y

summary(x)
summary(y)

z <- x+y
z

# jumlah
sum(x)
sum(y)

# nilai rata-rata
xbar <- sum(x)/length(x)
xbar

ybar <- sum(y)/length(y)
ybar

# simpangan baku
sx <- sqrt(sum((x-xbar)^2)/(length(x)-1))
sx
sd(x)

sy <- sqrt(sum((y-ybar)^2)/(length(y)-1))
sy
sd(y)

plot(x, y)
plot(x, y, col="red", pch=1, cex=50)

library(ggplot2)
df <- data.frame(x, y)
ggplot(data=df, aes(x=x, y=y))+
  geom_point(col="magenta", size=10, shape="star")+
  theme_bw()

# membuat matriks
D <- matrix(data=c(5,7,4,
                   6,8,7,
                   9,9,7),byrow=TRUE, ncol=3, nrow=3)
D

# mencari invers matriks
solve(D)
D%*%solve(D)
round(D%*%solve(D), 1) # membulatkan matriks

# mencari determinan matriks
det(D)

I3 <- diag(x=1, nrow=3, ncol=3)
I3

I3 <- diag(x=1, nrow=10, ncol=10)
I3

# Iris
data("iris")
summary(iris)
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
ggplot(data=iris, aes(x=Species, y=Sepal.Width, fill=Species))+
  geom_boxplot()+
  theme_bw()

p1<- ggplot(data=iris, aes(x=Species, y=Sepal.Length, fill=Species))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")

p2<- ggplot(data=iris, aes(x=Species, y=Sepal.Width, fill=Species))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")

p3<- ggplot(data=iris, aes(x=Species, y=Petal.Length, fill=Species))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")

p4<- ggplot(data=iris, aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4, ncol=4)

# menghitung nilai rata-rata Sepal Length untuk setiap bunga iris
tapply(X=iris$Sepal.Length, INDEX=iris$Species, FUN=mean)
aggregate(x=iris$Sepal.Length, by=list(iris$Species), FUN="mean")

# menghitung nilai simpangan baku Sepal Length untuk setiap bunga iris
tapply(X=iris$Sepal.Length, INDEX=iris$Species, FUN=sd)
aggregate(x=iris$Sepal.Length, by=list(iris$Species), FUN="sd")
