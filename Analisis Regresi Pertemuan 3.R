# Analisis Regresi Pertemuan 3

# Data
install.packages("readx1")
library(readxl)
toluca<-read_excel("C:/Users/LENOVO/OneDrive/Documents/Learning R/Semester 2/toluca.xlsx")
head(toluca)

# Eksplorasi Data
summary(toluca)
plot(toluca$LotSize, toluca$WorkHours,xlab="LotSize",ylab="WorkHours",pch=16)

# Persamaan Regresi
# Persamaan regresi menggunakan MKT dengan rumus
toluca$xdif = toluca$LotSize-mean(toluca$LotSize)
toluca$ydif = toluca$WorkHours-mean(toluca$WorkHours)
toluca$crp = toluca$xdif*toluca$ydif
toluca$xsq = toluca$xdif^2
toluca$ysq = toluca$ydif^2

b1 <- sum(toluca$crp)/sum(toluca$xsq)
b1

b0 <- mean(toluca$WorkHours) - b1*mean(toluca$LotSize)
b0

# Persamaan Regresi menggunakan fungsi lm pada R
# Intercept = b0, LotSize = b1
model2 <- lm(WorkHours~LotSize, data=toluca)
summary(model2)

plot(toluca$LotSize, toluca$WorkHours, pch=18, xlab="LotSize", ylab="WorkHours")
abline(model2, col="purple")

# Fitted values
model2$fitted.values

toluca_yhat <- b0 + b1*toluca$LotSize
head(toluca_yhat)

# Residuals
model2$residuals

toluca_residual<-toluca$WorkHours - model2$fitted.values
head(toluca_residual)

# ANOVA
JKG<-sum((toluca$WorkHours-toluca_yhat)^2)
JKG #JKG=SSE

JKR<-sum(((toluca_yhat-mean(toluca$WorkHours))^2))
JKR

JKT<-sum((toluca$WorkHours-mean(toluca$WorkHours))^2)
JKT

MSE = sum(toluca_residual^2)/(length(toluca$LotSize)-2) #KTG
MSE

KTG<-JKG/(length(toluca$LotSize)-2)
KTG

anova(model2)

# Uji Hipotesis β1 dan β0
# thitung b0 dan b1
KTG<-JKG/(length(toluca$LotSize)-2)
KTG

sb1sqr<-KTG/sum((toluca$LotSize-mean(toluca$LotSize))^2)
sb1<-sqrt(sb1sqr)
tb1<-b1/sb1
tb1

sb0sqr<-KTG*(1/length(toluca$LotSize)+((mean(toluca$LotSize)^2)/sum((toluca$LotSize-mean(toluca$LotSize))^2)))
sb0<-sqrt(sb0sqr)
tb0<-b0/sb0
tb0

# Selang Kepercayaan untuk β1 dan β0
#Selang kepercayaan
confint(model2,level=0.95)

#Selang Kepercayaan E{Yh}
xh1<-65
yh1<-b0+b1*xh1
syh1sqr<-KTG*(1/length(toluca$LotSize)+(((xh1-mean(toluca$LotSize))^2)/sum((toluca$LotSize-mean(toluca$LotSize))^2)))
syh1<-sqrt(syh1sqr)
Ttabel<-qt(0.975,length(toluca$LotSize)-2)

yh1_B<-yh1-Ttabel*syh1
yh1_A<-yh1+Ttabel*syh1
yh1_B # batas bawah
yh1_A # batas atas

#Prediksi
predxc1<-predict(model2,data.frame(LotSize = c(xh1)),interval = "confidence", se.fit=FALSE,level = 0.95)
predxc1

plot(toluca$LotSize, toluca$WorkHours)
abline(model2,col="magenta")
points(xh1, predxc1[, "fit"],col="red",pch=10)
points(xh1, predxc1[, "lwr"], lty = "dotted",col="red")
points(xh1, predxc1[, "upr"], lty = "dotted",col="red")
