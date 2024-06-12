---
  title: "Topik 2. Uji Khi-Kuadrat: Data Kategorik Univariat"
author: Sahda Huwaidah Estiningtyas
date: "`28 Februari 2024`"
---

# DATA 1
c1 <- c("Package bees","Nucs","Colonies","Swarms")
c2 <- c(31,36,26,20)
dat <- data.frame(kategori=c1,obs=c2)
str(dat)

# menghitung khi kuadrat
chisq <- chisq.test(dat$obs)
chisq

# menghitung p-value
p.value = 1-pchisq(q=4.9823,df=3)
p.value
khi_kuadrat_tabel <- qchisq(p=0.95,df=3)
khi_kuadrat_tabel


# DATA 2
c1 <- c("Turkey","Stuffing","Mashed potatoes","Yams","Green bean casserole","Cranberry sauce")
c2 <- c(250,148,98,55,30,42)
dat <- data.frame(kategori=c1,obs=c2)
str(dat)

chisq <- chisq.test(dat$obs,p =c(0.38,0.26,0.17,0.10,0.05,0.04))
chisq
p.value = 1-pchisq(q=15.144,df=5)
p.value
khi_kuadrat_tabel <- qchisq(p=0.95,df=5)
khi_kuadrat_tabel


# DATA 3
c1 <- c("Compact","Large","Midsize","Small", "Sporty", "Van")
c2 <- c(16,11,22,21,14,9)
dat <- data.frame(kategori=c1,obs=c2)
str(dat)

# menghitung khi kuadrat
chisq <- chisq.test(dat$obs)
chisq

# menghitung p-value
p.value = 1-pchisq(q=8.871,df=5)
p.value
