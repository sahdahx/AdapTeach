install.packages("ggplot2")
library(ggplot2)

# Membaca dataset
data <- read.csv("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/jumlah_balita.csv")
print(data)

# Membaca dataset dari file CSV
data <- read.csv("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/jumlah_balita.csv")

# Mengambil variabel wilayah, jumlah_balita_ditimbang, dan jumlah_balita_diukur_tinggi_badan untuk dianalisis
data_subset <- data[, c("wilayah", "jumlah_balita_ditimbang", "jumlah_balita_diukur_tinggi_badan")]

# Membuat diagram batang yang ditumpuk
data_long <- data.frame(
  wilayah = rep(data_subset$wilayah, 2),
  jumlah = c(data_subset$jumlah_balita_ditimbang, data_subset$jumlah_balita_diukur_tinggi_badan),
  tindakan = factor(rep(c("Ditimbang", "Diukur Tinggi Badan"), each = nrow(data_subset)))
)
ggplot(data_long, aes(x = wilayah, y = jumlah, fill = tindakan)) +
  geom_bar(stat = "identity") +
  labs(title = "Jumlah Balita yang Ditimbang dan Diukur Tinggi Badan per Wilayah", 
       x = "Wilayah", 
       y = "Jumlah Balita") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(name = "Tindakan", values = c("Ditimbang" = "purple", "Diukur Tinggi Badan" = "pink"))




# Menjumlahkan nilai dari seluruh wilayah
total_balita <- sum(data$jumlah_balita)
total_ditimbang <- sum(data$jumlah_balita_ditimbang)
total_diukur <- sum(data$jumlah_balita_diukur_tinggi_badan)
print(paste("total balita =", total_balita))
print(paste("total balita yang sudah ditimbang =", total_ditimbang))
print(paste("total balita yang sudah diukur =", total_diukur))

# Menghitung populasi balita yang belum ditimbang maupun diukur
total_belum_ditimbang_atau_diukur <- total_balita - total_ditimbang - total_diukur
print(paste("total balita yang belum diukur maupun ditimbang =", total_belum_ditimbang_atau_diukur))

# Menghitung persentase
persentase_ditimbang <- (total_ditimbang / total_balita) * 100
persentase_diukur <- (total_diukur / total_balita) * 100
persentase_belum_ditimbang_atau_diukur <- (total_belum_ditimbang_atau_diukur / total_balita) * 100

# Membuat dataframe untuk persentase
persentase <- data.frame(
  kategori = c("Ditimbang", "Diukur Tinggi Badan", "Belum Ditimbang/Diukur"),
  nilai = c(persentase_ditimbang, persentase_diukur, persentase_belum_ditimbang_atau_diukur)
)

# Membuat diagram lingkaran dengan label persentase
ggplot(persentase, aes(x = 1, y = nilai, fill = kategori, label = paste0(round(nilai), "%"))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  labs(title = "Persentase Balita Ditimbang, Diukur Tinggi Badan, dan Belum Ditimbang/Diukur",
       x = NULL, y = NULL) +
  scale_fill_manual(values = c("Ditimbang" = "pink", "Diukur Tinggi Badan" = "grey", "Belum Ditimbang/Diukur" = "yellow")) +
  theme_void()





# Membuat diagram garis
ggplot(data, aes(x = wilayah, y = jumlah_balita, group = 1, color = wilayah)) +
  geom_point() +
  geom_line() +
  labs(title = "Populasi Jumlah Bayi untuk 6 Wilayah",
       x = "Wilayah",
       y = "Jumlah Bayi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Menghitung total jumlah balita
total_balita <- sum(data$jumlah_balita)
print(total_balita)

# Menghitung persentase jumlah balita dari masing-masing wilayah
persentase_wilayah <- (data$jumlah_balita / total_balita) * 100

# Membuat dataframe untuk persentase wilayah
persentase_df <- data.frame(
  wilayah = data$wilayah,
  persentase = persentase_wilayah
)

ggplot(persentase_df, aes(x = "", y = persentase, fill = wilayah, label = sprintf("%.2f%%", persentase))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  labs(title = "Persentase Jumlah Bayi per Wilayah Terhadap Total 6 Wilayah",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Set3") +
  theme_void()
