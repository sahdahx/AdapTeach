# Tugas 2 Bagian A

# Membaca data
rmr <- read.table(file="C:/Users/LENOVO/OneDrive/Documents/Learning R/Semester 2/rmr.csv", header=TRUE, sep=",")
head(rmr)

# 1. Membuat plot metabolic rate vs body weight dan berikan interpretasi
attach(rmr)
plot(body.weight, metabolic.rate)

# 2. Menentukan variabel intependen dan variabel dependen
x <- body.weight #variabel independen
y <- metabolic.rate #variabel dependen

# 3. Melakukan pengepasan model regresi linear dan memberikan persamaan regresi dugaan dan interpretasinya
## Perhitungan menggunakan fungsi lm di program R
lm(metabolic.rate~body.weight)

# 4. Memprediksi metabolic rate bagi body weight sebesar 70kg
## Perhitungan menggunakan fungsi predict di program R
model_rmr <- lm(metabolic.rate~body.weight)
prediksi <- predict(model_rmr, data.frame(body.weight=c(70)))
prediksi

## Perhitungan secara manual di program R
x = 70
ytopi.prediksi = b0 + b1*x
ytopi.prediksi

# 5. Menentukan apakah ada hubungan linear positif antara metabolic rate dan body weight

# HIPOTESIS
### H0: p = 0 (Tidak ada hubungan linear antara berat badan (body.weight) dan laju metabolisme (metabolic.rate))
### H1: p ≠ 0  (Ada hubungan linear antara berat badan (body.weight) dan laju metabolisme (metabolic.rate))

# TARAF SIGNIFIKANSI
alpha = 0.05

# STATISTIK UJI
group1 <- rmr$body.weight
group2 <- rmr$metabolic.rate
thitung <- t.test(group1, group2)
thitung 

# KRITERIA KEPUTUSAN
## menghitung t tabel
ttabel <- qt(0.975, 42)
ttabel

### H0 ditolak jika |t hitung| > t tabel=2.018082, atau
### H0 ditolak jika p-value < 0.05

# HITUNGAN
## Perhitungan menggunakan fungsi cor.test di program R
cor.test(body.weight, metabolic.rate)

## Perhitungan p-value
n <- nrow(rmr)
p.value <- 1-pf(fhitung, df1=1, df2=n-2)
p.value







# 6. Membuat interval kepercayaan 95% bagi slope dan memberikan interpretasi
## Perhitungan b1 dan b0 secara manual di program R
sum.x <- sum(body.weight)
sum.x

sum.x2 <- sum(body.weight^2)
sum.x2

sum.y <- sum(metabolic.rate)
sum.y

sum.xy <- sum(body.weight*metabolic.rate)
sum.xy

n <- nrow(rmr)
n

b1 <- (n*sum.xy - sum.x*sum.y)/(n*sum.x2 - (sum.x)^2)
b1

b0 <- (sum.y - b1*sum.x)/n
b0

ttabel <- qt(0.975, 42)
ttabel

Sxx <- sum((body.weight-mean(body.weight))^2)
Sxx

Syy <- sum((metabolic.rate-mean(metabolic.rate))^2)
Syy

Sxy <- sum((body.weight-mean(body.weight))*(metabolic.rate-mean(metabolic.rate)))
Sxy

SST <- Syy
SST

SSR <- b1*(Sxy)
SSR

SSE <- SST-SSR
SSE

MSR <- SSR/1
MSR

MSE <- SSE/(n-2)
MSE

sb1.sqr <- MSE/Sxx
sb1 <- sqrt(sb1.sqr)
sb1

batas_bawah = b1-(ttabel*sb1)
batas_bawah

batas_atas = b1+(ttabel*sb1)
batas_atas

# 7. Menentukan koefisien korelasi dan koefisien determiasi antara metabolic rate dan boy weight dan memberikan interpretasi
## Koefisien Determinasi (R^2)
R2 <- SSR/SST
R2

## Koefisien Korelasi
R <- sqrt(R2)
R

# 8. Melakukan diagnostik regresi terkait dengan asumsi-asumsi dalam model regresi linear sederhana
## Plot nilai dugaan vs residual dan plot X vs residual
model_rmr <- lm(metabolic.rate~body.weight)
par(mfrow=c(1,2))
plot(fitted(model_rmr), resid(model_rmr), xlab="Nilai dugaan", ylab="Residual")
abline(h=0,col="blue",lty=2)
plot(body.weight, resid(model_rmr), xlab="X", ylab="Residual")
abline(h=0,col="red",lty=2)
