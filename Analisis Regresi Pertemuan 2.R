# Analisis Regresi Pertemuan 2

# Data
install.packages("readx1")
library(readxl)
salary <- read.csv("C:/Users/LENOVO/OneDrive/Documents/Learning R/Semester 2/Salary_dataset.csv")
salary<-salary[,-1]
head(salary)

# Eksplorasi Data
plot(salary$YearsExperience, salary$Salary,xlab="Years Experience",ylab="Salary",col="magenta", pch=16)

# Persamaan Regresi dengan MKT manual
salary$xdif <- salary$YearsExperience-mean(salary$YearsExperience)
salary$ydif <- salary$Salary-mean(salary$Salary)
salary$crp <- salary$xdif*salary$ydif
salary$xsq <- salary$xdif^2

#estimator b0 dan b1
b1 <- sum(salary$crp)/sum(salary$xsq)
b1
#Parameter Estimates
b0 <- mean(salary$Salary) - b1 * mean(salary$YearsExperience)
b0

# Persamaan Regresi dengan R function
model1<-lm(Salary~YearsExperience,data=salary)
summary(model1)

# Plot Regresi
plot(salary$YearsExperience, salary$Salary,xlab="YearsExperience",ylab="Salary",pch=16)
abline(model1)

# Koefisien Determinasi
JKG<-sum((salary$Salary-model1$fitted.values)^2)
JKG

JKR<-sum(((model1$fitted.values-mean(salary$Salary))^2))
JKR

JKT<-sum((salary$Salary-mean(salary$Salary))^2)
JKT

# koefisien determinasi
r_sqr<-(JKT-JKG)/JKT
r_sqr

# koefisien korelasi
sqrt(r_sqr)

# ANOVA
anova(model1)

##koefisien korelasi
x1_x1bar<-salary$YearsExperience-mean(salary$YearsExperience)
x2_x2bar<-salary$Salary-mean(salary$Salary)
A<-sum(x1_x1bar*x2_x2bar)
varx1<-sum((x1_x1bar)^2)
varx2<-sum((x2_x2bar)^2)
B<-sqrt(varx1*varx2)
corr<-A/B
corr
#atau gunakan fungsi cor
cor(salary$YearsExperience,salary$Salary)

# Uji Signifikansi Korelasi
#Pearson Correlation test, alpha 1%
cor.test( ~ YearsExperience + Salary, data=salary, method = "pearson", continuity = TRUE, conf.level = 0.99)
