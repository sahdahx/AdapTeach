# UAS BAGIAN PERTAMA

# Data Observasi dan Harapan
observed <- matrix(c(32, 30, 28, 74, 64, 42, 110, 25, 15, 39, 6, 5), nrow=4, byrow=TRUE)
expected <- matrix(c(48.8, 23.9, 17.2, 97.7, 47.9, 34.5, 81.4, 39.9, 28.7, 27.1, 13.3, 9.6), nrow=4, byrow=TRUE)
# Menghitung nilai chi-square
chi_square <- sum((observed - expected)^2 / expected)
chi_square

x <- c(1,3,5)
y <- c(3,2)
cbind(x,y) 

library(lattice)
library(ggplot2)
data(iris)
# Membuat stripplot
stripplot(iris$Sepal.Length ~ iris$Species, 
          xlab = "Species", ylab = "Sepal Length",
          main = "Distribusi Panjang Sepal untuk Setiap Spesies Bunga Iris")

