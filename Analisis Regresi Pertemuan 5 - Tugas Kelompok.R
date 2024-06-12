# Analisis Uji Ketidakpasan dan Uji Asumsi Regresi Linier Sederhana mengenai hubungan antara usia Height dan HandSpan pada data handheight

# Membaca Data
handheight<-read.table("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/handheight.txt", header = TRUE)
head(handheight)

# Eksplorasi data
summary(handheight)
plot(handheight$Height, handheight$HandSpan, xlab="Height", ylab="HandSpan", pch=10)

# Persamaan regresi menggunakan MKT dengan rumus
handheight$xdif = handheight$Height-mean(handheight$Height)
handheight$ydif = handheight$HandSpan-mean(handheight$HandSpan)
handheight$crp = handheight$xdif*handheight$ydif
handheight$xsq = handheight$xdif^2
handheight$ysq = handheight$ydif^2

b1 <- sum(handheight$crp)/sum(handheight$xsq)
b1

b0 <- mean(handheight$HandSpan) - b1*mean(handheight$Height)
b0

# Persamaan Regresi menggunakan fungsi lm pada R
model2 <- lm(HandSpan ~ Height, data = handheight)
summary(model2)

# Asumsi 1: Rata-rata galat diasumsikan bernilai nol
plot(handheight$Height, model2$residuals, 
     xlab="Height", ylab="HandSpan", pch=10, 
     main="Plot Uji Asumsi Rata-rata Galat bernilai nol")

# Asumsi 2: Galat saling bebas
c <- (1:167)
handheight <- cbind(handheight, c)
head(handheight)

plot(handheight$c, model2$residuals,
     xlab = "amatan", ylab = "residuals", type = "l",
     main = "Plot Uji Asumsi Galat Saling Bebas")

plot(handheight$c,model2$residuals,
     xlab="Amatan",ylab="Residuals",
     main="Plot Uji Asumsi Galat Saling Bebas")

# Asumsi 3: Galat berdistribusi normal
c <- (1:167)
ytopi <- model2$fitted.values
ei <- model2$residuals
eiterurut <- sort(model2$residuals)
anova(model2)

hi<-sqrt(1.69)*qnorm((c-0.375)/(167+0.25))
Hi<-cbind(handheight, ytopi, ei, eiterurut, hi)
head(Hi)

plot(hi,eiterurut,
     xlab="hi",ylab="eiterurut",
     main="Plot Uji Galat Berdistribusi Normal")

### Atau gunakan fungsi qqnorm dan qqline di R berikut
qqnorm(model2$residuals, ylab = "raw residuals")
qqline(model2$residuals)

## Histogram
hist(model2$residuals, main="Histogram Uji Galat Berdistribusi Normal")
## Histogram untuk interval data = 167
hist(model2$residuals, 167, main="Histogram Uji Galat Berdistribusi Normal untuk Interval Data = 167")
## Boxplot
boxplot(model2$residuals, main="Boxplot")

# Asumsi 4: Ragam galat diasumsikan konstan
ytopi<-model2$fitted.values
ei<-model2$residuals
plot(ytopi,ei,
     xlab="fitted values", ylab="residuals",
     main="Plot Uji Ragam Galat Konstan")

# Asumsi 5: X dan Y berhubungan linear
plot(handheight$Height, handheight$HandSpan, main="Plot Data")

# Asumsi 6: Tidak ada outlier
plot(handheight$Height, handheight$HandSpan, main="Plot Data")

