---
title: "Tugas 1 - Topik 3. Uji Khi-Kuadrat: Data Kategorik Bivariat"
author: Sahda Huwaidah Estiningtyas
date: "`6 Maret 2024`"
---

# NOMOR 1
# Membuat data frame
pekerjaan <- c("Manajemen", "Bisnis, keuangan, administrasi", " Ilmu pengetahuan alam dan terapan", "Kesehatan", 
"Ilmu sosial, pendidikan, pemerintahan", "Seni, budaya, rekreasi, dan olahraga", "Penjualan dan servis", " Perdagangan, transportasi, operator", "Lainnya")
nilai_observasi <- c(1503, 3517, 1357, 1183, 1659, 595, 4262, 2634, 1345)
data1 <- data.frame(kategori=pekerjaan, obs=nilai_observasi)
str(data1)

# Menentukan chi-squared
chisq <- chisq.test(data1$obs, p=c(0.09, 0.18, 0.07, 0.07, 0.09, 0.03, 0.24, 0.15, 0.08))
chisq

# Menentukan P-value
p.value = 1-pchisq(q=59.156, df=8)
p.value

# chi-squared tabel
khi_kuadrat_tabel <- qchisq(p=0.99,df=9)
khi_kuadrat_tabel

# NOMOR 2: mencari khi-kuadrat tabel
# a
khi_kuadrat_tabel_a <- qchisq(p=0.95,df=6)
khi_kuadrat_tabel_a

# b
khi_kuadrat_tabel_b <- qchisq(p=0.99,df=5)
khi_kuadrat_tabel_b

# c
khi_kuadrat_tabel_c <- qchisq(p=0.975,df=6)
khi_kuadrat_tabel_c

# d
khi_kuadrat_tabel_d <- qchisq(p=0.999,df=8)
khi_kuadrat_tabel_d

# NOMOR 5
atlet <- array(data=c(12,335,17,292,25,288), dim=c(2,3),
               dimnames=list(First=c("ya","tidak"), Second=c("rendah","sedang","tinggi")))
atlet

chisq <- chisq.test(atlet)
chisq

chisq$expected
colSums(atlet)
rowSums(atlet)
sum(colSums(atlet))
sum(rowSums(atlet))
p.value = 1-pchisq(q=6.4189,df=2)
p.value

khi_kuadrat_tabel <- qchisq(p=0.95,df=2)
khi_kuadrat_tabel

# NOMOR 7
# Memanggil data ke dalam R
dat3<-read.table(file="C:/Users/LENOVO/OneDrive/Documents/Learning R/Semester 2/psycparole.txt",head=FALSE, 
                 col.names=c("Psychiatric Prognosis", "Parole adjustment", "Count"))
dat3

# mengubah variabel menjadi faktor
dat3$Psychiatric.Prognosis<- factor(dat3$Psychiatric.Prognosis,levels=1:3,labels=c("Unfavorable", "Doubtful", "Favorable "))
dat3$Parole.adjustment<- factor(dat3$Parole.adjustment,levels=1:3,labels=c("Major violation", "Minor violation", "Success"))
dat3

# Menampilkan struktur data
str(dat3)

# Membuat tabel kontingensi
tab <- tapply(dat3$Count,list(dat3$Psychiatric.Prognosis, dat3$Parole.adjustment),c)
names(dimnames(tab))<-c("Psychiatric.Prognosis","Parole.adjustment")
tab

# Melakukan pengujian dengan statistik khi-kuadrat
chisq <- chisq.test(tab)
chisq


# Menampilkan sel harapan
chisq$expected

khi_kuadrat_tabel<-qchisq(p=0.99,df=4)
khi_kuadrat_tabel
