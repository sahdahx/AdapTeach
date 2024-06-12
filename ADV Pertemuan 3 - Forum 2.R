# FORUM 2
install.packages("readxl")
library(readxl)

# Membaca data
data1 <- read.table(file="C:/Users/LENOVO/Downloads/BankWages.csv", header=TRUE, sep=",")
data1

# Membuat tabel frekuensi dua arah
frequency_table <- table(data1$job, data1$gender)
frequency_table

data2 <- array(data = frequency_table, 
                     dim = c(3,2), 
                     dimnames = list(job = c("admin","custodial","manage"), gender = c("female", "male")))
data2

# Visualisasi data dengan side-by-side bar chart
par(mfrow=c(1,3))
barplot(data2[1,],xlab="Gender",ylab="Frequency",main="admin")
barplot(data2[2,],xlab="Gender type",ylab="Frequency",main="custodial")
barplot(data2[3,],xlab="Gender type",ylab="Frequency",main="manage")

par(mfrow=c(1,1))
barplot(data2,legend=TRUE,beside=TRUE,xlab="Gender",ylab="Frequency",args.legend = list(x = "top"))

# Visualisasi data dengan stacked bar chart
barplot(t(data2),xlab="Gender",ylab="Frequency",ylim=c(0,400), legend=TRUE,args.legend = list(x = "top"))

# UJI HOMOGENITAS POPULASI

# menghitung statistik uji
chisq <- chisq.test(data2)
chisq

# menghitung p-value
p.value = 1-pchisq(q=79.277,df=2)
p.value

# mencari khi-kuadrat tabel
khi_kuadrat_tabel <- qchisq(p=0.95,df=2)
khi_kuadrat_tabel

chisq$expected
colSums(data2)
rowSums(data2)
sum(colSums(data2))
sum(rowSums(data2))