# UAS Statistika Nonparametrik, 11 Juni 2024

# NOMOR 1
# Data
kelompok <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)
skor <- c(5.0, 6.1, 4.3, 5.0, 5.3, 5.5, 5.4, 6.2, 5.8, 5.5, 5.2, 4.7, 6.4, 6.8, 6.5, 6.0, 6.3, 6.6)
data <- data.frame(kelompok, skor)
data
kruskal.test(skor~kelompok, data=data)

# Menentukan ranking skor dalam setiap kelompok
data$ranking <- rank(data$skor, ties.method = "min")
print(data)


# NOMOR 2
# Data
data <- matrix(c(20, 154, 110, 53, 372, 212, 90, 274, 82), 
               nrow = 3, 
               byrow = TRUE,
               dimnames = list("Pendapatan" = c("Di atas rata-rata", "Rata-rata", "Di bawah rata-rata"),
                               "Kebahagiaan" = c("Tidak Terlalu Bahagia", "Cukup Bahagia", "Sangat Bahagia")))

data
# Melakukan uji Chi-Square
chi_square_test <- chisq.test(data)
chi_square_test

# Chi-Square tabel
alpha <- 0.10
df <- 4
chi_critical <- qchisq(1 - alpha, df)
chi_critical


# NOMOR 4
# Data
kekuatan_lengan <- c(17.0, 19.2, 18.5, 19.2, 22.3, 23.1, 29.4, 26.8, 26.7, 28.2,
                     28.6, 28.7, 29.5, 29.6, 29.9, 29.9, 30.3, 33.3, 36.0, 39.5,
                     40.6, 44.9, 46.6)
angkatan_dinamis <- c(71.7, 48.3, 88.3, 75.0, 91.7, 100.0, 73.3, 65.0, 75.0, 88.3,
                      68.3, 96.7, 76.7, 78.3, 60.0, 71.7, 85.0, 85.0, 88.3, 100.0,
                      100.0, 100.0, 91.7)

# Menghitung koefisien korelasi Spearman
correlation <- cor(kekuatan_lengan, angkatan_dinamis, method = "spearman")
correlation

# Uji Hipotesis
# Hipotesis nol (H0): Tidak ada hubungan antara kekuatan lengan dengan angkatan dinamis
# Hipotesis alternatif (H1): Terdapat hubungan antara kekuatan lengan dengan angkatan dinamis

# Taraf Signifikansi
alpha <- 0.05
n <- length(kekuatan_lengan)

# Uji Statistik
statistic <- correlation * sqrt((n - 2) / (1 - correlation^2))
statistic
# Nilai Kritis
critical_value <- qt(1 - alpha/2, df = n - 2)
critical_value

# Menentukan keputusan
if (abs(statistic) > critical_value) {
  cat("Tolak H0, terdapat hubungan yang signifikan antara kekuatan lengan dengan angkatan dinamis.")
} else {
  cat("Terima H0, tidak terdapat hubungan yang signifikan antara kekuatan lengan dengan angkatan dinamis.")
}


# NOMOR 5
# Data
curah_hujan <- c(2.1, 3.7, 4.3, 4.5, 5.0, 5.6, 5.8, 6.2)
zat_terbawa <- c(108, 115, 116, 127, 126, 137, 140, 133)

data <- data.frame(Curah_Hujan = curah_hujan, Zat_Terbawa = zat_terbawa)
data

# Menghitung koefisien korelasi rank Kendall (τ)
kendall_tau <- cor.test(data$Curah_Hujan, data$Zat_Terbawa, method = "kendall")
kendall_tau

# Ambil nilai p-value
p_value <- cor_test$p.value
p_value

# Lakukan pengujian hipotesis
if (p_value < alpha) {
  cat("Tolak H0: Terdapat hubungan yang signifikan antara banyaknya curah hujan dan jumlah kotoran udara yang terbawa hujan.")
} else {
  cat("Tidak cukup bukti untuk menolak H0: Tidak terdapat hubungan yang signifikan antara banyaknya curah hujan dan jumlah kotoran udara yang terbawa hujan.")
}

