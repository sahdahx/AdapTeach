# Anggota kelompok G:
# - Safira Wafda Danantika (23031030009)
# - Nariza Rahmadani P.M (23031030012)
# - Sahda Huwaidah Estiningtyas (23031030019)
# - Qurrota A'yun Zahirah  (23031030029)

# Membaca data
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/Parade2005.csv", head=TRUE)
str(dat)

cat.car <- table(dat$gender)
cat.car

# Menghitung khi kuadrat
chisq <- chisq.test(cat.car)
chisq

# Menghitung p-value
p.value = 1-pchisq(q=0.27692,df=1)
p.value

# KESIMPULAN:
# Oleh karena 𝜒2 = 0,27692 < 3,841 atau p-value = 0.5987271 > 0.05 maka H0 tidak ditolak. 
# Pada taraf signifikansi 𝛼 = 0.05 dapat disimpulkan bahwa proporsi kategori female dan male adalah sama.








# Untuk membutikan perhitungan khi kuadrat manual
# Menghitung khi kuadrat untuk 5 data teratas
top5_data <- head(dat, 5)
print(top5_data)
chisq_top5 <- chisq.test(top5_data$obs)
chisq_top5
