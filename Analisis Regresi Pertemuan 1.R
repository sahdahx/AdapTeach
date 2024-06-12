# Regresi Linear, Pertemuan 1
# sumber: https://rpubs.com/andikaputri/P1-Anreg
pendapatan <- c(15,20,25,20,25,30,16,15,25,20,16,18,20,25,30,25,19,10,20,20)
pengeluaran <- c(10,15,20,16,22,25,15,14,10,18,12,15,15,20,25,23,16,8,15,17)
data1 <- data.frame(pendapatan, pengeluaran) #membuat dataframe
head(data1)

plot(data1$pendapatan, data1$pengeluaran, main="Scatterplot",xlab="pendapatan", ylab="pengeluaran", pch=19)

par(mfrow=c(1, 2)) # divide graph area in 2 columns
boxplot(data1$pendapatan, main="Pendapatan", sub=paste("Outlier rows: ", boxplot.stats(data1$pendapatan)$out)) # box plot for 'pendapatan'
boxplot(data1$pengeluaran, main="Pengeluaran", sub=paste("Outlier rows: ", boxplot.stats(data1$pengeluaran)$out)) # box plot for 'pengeluaran'
cor(data1$pendapatan, data1$pengeluaran) # calculate correlation between pendapatan and pengeluaran 

data1$xdif <- data1$pendapatan-mean(data1$pendapatan)
data1$ydif <- data1$pengeluaran-mean(data1$pengeluaran)
data1$crp <- data1$xdif * data1$ydif
data1$xsq <- data1$xdif^2
#estimator b0 dan b1
b1 <- sum(data1$crp)/sum(data1$xsq)
b1

#Parameter Estimates
b0 <- mean(data1$pengeluaran) - b1 * mean(data1$pendapatan)
b0

#gunakan perintah help(lm) untuk mengetahui secara detail suatu fungsi dalam R
lm1 <- lm(pengeluaran ~ pendapatan, data=data1) # build linear regression model on full data
summary(lm1)

plot(data1$pendapatan,data1$pengeluaran,main="Pengeluaran ~ Pendapatan",xlab="Pendapatan",ylab="Pengeluaran", pch=19)
abline(lm1,col="blue")


# Data baru
pH <- c(3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.0, 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5.0, 5.1, 5.2)
Grow_Ret <- c(17.78, 21.59, 23.84, 15.13, 23.45, 20.87, 17.78, 20.09, 17.78, 12.46, 14.95, 15.87, 17.45, 14.35, 14.64, 17.25, 12.57, 7.15, 7.50, 4.34)
data2 <- data.frame(pH, Grow_Ret) #membuat dataframe
head(data2)

# Membuat model regresi linier
regression_model <- lm(Grow_Ret ~ pH, data=data2)

# Menampilkan hasil regresi
summary(regression_model)

pH_prediksi <- 4.9
indeks_pertumbuhan_prediksi <- predict(regression_model, newdata = data.frame(pH = pH_prediksi))
cat("Prediksi Indeks Pertumbuhan untuk pH =", pH_prediksi, "adalah", indeks_pertumbuhan_prediksi, "\n")