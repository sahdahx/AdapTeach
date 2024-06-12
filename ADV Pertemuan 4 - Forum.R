---
title: "Tugas Forum - Topik 4. Regresi Linear Sederhana"
author: Sahda Huwaidah Estiningtyas
date: "`6 Maret 2024`"
---

# membaca data
cav <- read.csv(file="C:/Users/LENOVO/OneDrive/Documents/Learning R/Semester 2/cav.csv", head=TRUE)
cav

# mengidentifikasi kelengkapan baris dari suatu data frame
complete.cases(cav) 

cav.complete <- cav[complete.cases(cav), ]
cav.complete

# Apakah ada hubungan linear antara posisi koordinat x dan y?
attach(cav.complete)
plot(x, y)

sum.x <- sum(x)
sum.x

sum.x2 <- sum(x^2)
sum.x2

sum.y <- sum(y)
sum.y

sum.xy <- sum(x*y)
sum.xy

n <- nrow(cav.complete) #n=138
n

beta1.hat <- (n*sum.xy-sum.x*sum.y)/(n*sum.x2-(sum.x)^2)
beta1.hat

beta0.hat <- (sum.y-beta1.hat*sum.x)/n
beta0.hat

# menggunakan fungsi lm di R
lm(y~x)

# prediksi
plot(x,y)
abline(lm(y~x))

# Sum of squares
Sxx <- sum((x-mean(x))^2)
Sxx

Syy <- sum((y-mean(y))^2)
Syy

Sxy <- sum((x-mean(x))*(y-mean(y)))
Sxy

SST <- Syy
SST

SSR <- beta1.hat*(Sxy)
SSR

SSE <- SST-SSR
SSE

MSR <- SSR/1
MSR

MSE <- SSE/(n-2)
MSE

# F hitung atau F statistics
F <- MSR/MSE
F

p.value <- 1-pf(F,df1=1,df2=n-2)
p.value

R2 <- SSR/SST
R2 # Koefisien determinasi R-kuadrat = 0.02254224 berarti 2% keragaman dalam data dijelaskan oleh model regresi

# Tabel ANOVA dengan program R
mod.reg <- lm(y~x)
mod.aov <- anova(mod.reg)
mod.aov

# F tabel
qf(0.95,df1=1,df2=136)

# Uji hipotesis dan interval kepercayaan bagi beta1
mod.reg <- lm(y~x)
summary(mod.reg)
confint(mod.reg)

# koefisien korelasi sampel
r <- Sxy/sqrt(Sxx*Syy)
r
cor(x,y)

# koefisien korelasi
cor.test(x,y)

# Plot nilai dugaan vs residual & plot X vs residual
par(mfrow=c(1,2))
plot(fitted(mod.reg),resid(mod.reg),xlab="Nilai dugaan",ylab="Residual")
abline(h=0,col="blue",lty=2)
plot(x,resid(mod.reg),xlab="X",ylab="Residual")
abline(h=0,col="blue",lty=2)

# plot peluang normal
qqnorm(resid(mod.reg))

# PADA DATA CAV

# Hipotesis
# H0: Tidak ada hubungan linear yang signifikan antara gula darah dan kecepatan pemendekan ventrikel (𝛽1 = 0)
# H1: Ada hubungan linear yang signifikan antara gula darah dan kecepatan pemendekan ventrikel (𝛽1 ≠ 0)

# Taraf signifikansi: 
# 𝛼 = 0.05

# Statistik uji: 
# 𝐹 = 𝑀𝑆𝑅/𝑀𝑆𝐸

# Kriteria keputusan: 
# 𝐹0.05(1,136) = 3.910747
# H0 ditolak jika 𝐹 > 3.910747 atau H0 ditolak jika p-value < 0.05

# Hitungan: 
# F = 3.136447 dan p-value = 0.0788  

# Kesimpulan:
# Oleh karena F = 3.136447 < 3.910747 (atau p-value = 0.0788 > 0.05) maka H0 diterima. Jadi pada taraf signifikansi 0.05 dapat disimpulkan bahwa tidak ada hubungan linear yang signifikan antara posisi koordinat x dan koordinat y.
