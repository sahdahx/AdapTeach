# Analisis Regresi Pertemuan 6

# Data
library(readxl)
bodyf<- read_excel("C:/Users/LENOVO/OneDrive/Documents/Learning R/Semester 2/data_bodyfat.xlsx")
print.data.frame(bodyf[,-1]) #menghapus kolom 1

# X1: Triceps Skinfold Thickness
# X2: Thigh Circumference
# X3: Midarm Circumference
# Y: Body Fat

# Persamaan Regresi
modela<- lm(formula = Bodyfat_Y ~ Triceps_X1+Thigh_X2+Midarm_X3, data=bodyf)
summary(modela)

# ANOVA
anova (modela)

# Interval Kepercayaan
confint (modela, level = 0.95)

# Persamaan Regresi dengan Matriks
Y<-matrix(bodyf$Bodyfat_Y)
N<-nrow(Y) #mengetahui jumlah baris Y
X<-matrix(1,N)
X<-cbind(X,bodyf$Triceps_X1,bodyf$Thigh_X2, bodyf$Midarm_X3)
J<-matrix(1,N,N)
Xt<-t(X)
A<-Xt %*% X
A

B<-solve(A)
B

C<- Xt %*% Y
C

Beta<-B %*% C
Beta

# Inferensi
JKT<-(t(Y)%*%Y)-((1/N)*(t(Y)%*%J%*%Y))
JKT

JKG<-(t(Y)%*%Y)-(t(Beta)%*%t(X)%*%Y)
JKG

JKR<-JKT-JKG
JKR

fhitung<-(JKR/(3-1))/(JKG/(N-3))
fhitung

qf(0.99,(4-1),(N-4)) #alfa=1%

KTG<-JKG/(N-3)
KTG<-as.numeric(KTG)
KTG

Sb<-KTG*solve(t(X)%*%X)
Sb

thitung0<-Beta[1,1]/sqrt(Sb[1,1])
thitung0

thitung1<-Beta[2,1]/sqrt(Sb[2,2])
thitung1

