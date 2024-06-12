---
title: "Topik 3. Uji Khi-Kuadrat: Data Kategorik Bivariat"
author: Sahda Huwaidah Estiningtyas
date: "`6 Maret 2024`"
---

# Membuat tabel frekuensi dua arah
trade.table <- array(data = c(15,22,32,25,24,25,30,15,20,22,30,40), 
                     dim = c(3,4), 
                     dimnames = list(First = c("Fidelity","Vanguard","WellsTrade"), Second = c("Agriculture", "Energy","Financials","Metals")))
trade.table

# Visualisasi data dengan side-by-side bar chart
par(mfrow=c(1,3))
barplot(trade.table[1,],xlab="Futures type",ylab="Frequency",main="Fidelity")
barplot(trade.table[2,],xlab="Futures type",ylab="Frequency",main="Vanguard")
barplot(trade.table[3,],xlab="Futures type",ylab="Frequency",main="WellsTrade")

par(mfrow=c(1,1))
barplot(trade.table,legend=TRUE,beside=TRUE,xlab="Futures type",ylab="Frequency",args.legend = list(x = "top"))

# Visualisasi data dengan stacked bar chart
barplot(t(trade.table),xlab="Futures type",ylab="Frequency",ylim=c(0,200), legend=TRUE,args.legend = list(x = "top"))

# UJI HOMOGENITAS POPULASI

# menghitung statistik uji
chisq <- chisq.test(trade.table)
chisq

# menghitung p-value
p.value = 1-pchisq(q=13.009,df=6)
p.value

# mencari khi-kuadrat tabel
khi_kuadrat_tabel <- qchisq(p=0.95,df=6)
khi_kuadrat_tabel

chisq$expected
colSums(trade.table)
rowSums(trade.table)
sum(colSums(trade.table))
sum(rowSums(trade.table))

# UJI INDEPENDENSI

flu <- array(data = c(207,66,319,300,120,849,630,1207,1196,3599,1592,754,1617,1946,5860,617, 895,512,510,597), 
             dim = c(5,4), 
             dimnames = list(First = c("<5","5-19","20-44","45-64","65+"), Second = c("A/H1N1", "A/H3N2","Aunsub","B")))
flu

chisq <- chisq.test(flu)
chisq
chisq$expected

colSums(flu)
rowSums(flu)

sum(colSums(flu))
sum(rowSums(flu))

p.value = 1-pchisq(q=2505.6,df=12)
p.value

khi_kuadrat_tabel <- qchisq(p=0.99,df=12)
khi_kuadrat_tabel

# SMOKING, AGE, AND DEATH (DATA)
#membaca data ke dalam program R
dat1 <- read.table(file="C:/Users/LENOVO/OneDrive/Documents/Learning R/Semester 2/smokedata.dat.txt", head=TRUE, col.names=c("Smoking","Age","Death","NoC"))

#membuat variabel menjadi faktor
dat1$Smoking <- factor(dat1$Smoking,levels=0:1,labels=c("No", "Yes"))
dat1$Age <- factor(dat1$Age,levels=1:4,labels=c("50-54", "55-59","60-64","65-69"))
dat1$Death <- factor(dat1$Death,levels=0:1,labels=c("Survival", "Death"))

#mensubset data bagi pasien yang meninggal
dat2 <- dat1[which(dat1$Death=="Death"),]

#menampilkan struktur data
str(dat2)

#membuat tabel kontingensi berdasarkan banyaknya pengamatan
tab<- tapply(dat2$NoC,list(dat2$Smoking,dat2$Age),c)
names(dimnames(tab)) <- c("Smoking","Age")
tab

#melakukan pengujian dengan statistik khi-kuadrat
chisq <-chisq.test(tab)
chisq

#menampilkan sel harapan
chisq$expected