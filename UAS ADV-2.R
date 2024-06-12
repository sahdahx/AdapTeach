# UAS BAGIAN KEDUA
library(ggplot2)
library(ggridges)
library(gridExtra)
library(viridis)
library(dplyr)
library(tidyverse)

# Membaca data txhousing
txhousing <- read_csv("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/txhousing.csv")
View(txhousing)

# 1. Membuat plot pencar beserta garis regresi untuk tahun 2000-2015
filtered_data <- txhousing %>%
  filter(city %in% c("Dallas", "Houston") & year %in% c(2000, 2015))

ggplot(filtered_data, aes(x = sales, y = volume, color = city)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~year) +
  labs(title = "Hubungan antara Jumlah Rumah Terjual dan Total Nilai Penjualan Rumah",
       subtitle = "Di Kota Dallas dan Houston pada Tahun 2000 dan 2015",
       x = "Jumlah Rumah Terjual (Sales)",
       y = "Total Nilai Penjualan Rumah (Volume)",
       color = "Kota") +
  theme_minimal()

# 2. Pada bulan Desember 2014, kota mana yang memiliki rumah paling sedikit yang terdaftar (listings) untuk dijual?
dec_2014_data <- txhousing %>%
  filter(year == 2014 & month == 12)
city_with_fewest_listings <- dec_2014_data %>%
  filter(listings == min(listings, na.rm = TRUE))
city_with_fewest_listings

# 5. Manakah barplot yang menunjukkan tren penjualan rumah (sales) bulanan pada tahun 2010 di kota Houston berdasarkan data ini? 
houston_2010 <- txhousing %>%
  filter(city == "Houston" & year == 2010)
# Membuat barplot tren penjualan rumah bulanan
ggplot(houston_2010, aes(x = factor(month), y = sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Tren Penjualan Rumah Bulanan di Houston pada Tahun 2010",
       x = "Bulan",
       y = "Jumlah Penjualan (Sales)") +
  theme_minimal()

# 6. Pada tahun 2012, pada bulan apa rumah paling banyak terjual di kota Houston?
txhousing %>%
  filter(city == "Houston" & year == 2012) %>%
  filter(sales == max(sales)) %>%
  pull(month)

# 11. Pada tahun berapakah rata-rata persentase rumah terjual yang terdaftar (sales/listings) di Austin mencapai nilai tertinggi? 
txhousing %>%
  filter(city == "Austin") %>%
  group_by(year) %>%
  summarise(avg_ratio = mean(sales / listings, na.rm = TRUE)) %>%
  filter(avg_ratio == max(avg_ratio)) %>%
  pull(year)

# 15. Manakah barplot yang menunjukkan tren penjualan rumah (sales) di kota Dallas berdasarkan data ini? 
dallas_data <- txhousing %>% filter(city == "Dallas")

ggplot(dallas_data, aes(x = factor(year), y = sales)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Housing Sales in Dallas",
       x = "Tahun",
       y = "Jumlah Penjualan (sales)") +
  theme_minimal()

# 16. Manakah plot pencar berikut yang menunjukkan hubungan antara total rumah terdaftar aktif (listings) dan jumlah rumah yang terjual (sales) di kota Arlington berdasarkan data ini? 
arlington_data <- txhousing %>%
  filter(city == "Arlington", year >= 2000, year <= 2015)

ggplot(arlington_data, aes(x = listings, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between Active Listings and Sales in Arlington (2000-2015)",
       x = "Total Active Listings",
       y = "Number of Houses Sold") +
  facet_wrap(~ year, ncol = 4) +
  theme_minimal()


# 17. Pada tahun 2008, manakah lima kota yang memiliki total nilai penjualan rumah (volume) terbesar?
top_cities_2008 <- txhousing %>%
  filter(year == 2008) %>%
  group_by(city) %>%
  summarise(total_volume = sum(volume, na.rm = TRUE)) %>%
  arrange(desc(total_volume)) %>%
  slice_head(n = 5)
top_cities_2008

# 20. Manakah kode R berikut yang digunakan untuk untuk membatasi kumpulan data sehingga hanya beberapa tahun sebelum 2004 dan kota San Antonio?
txhousing %>% filter(year < 2004, city == "San Antonio")



# Membaca data weather
weather <- read_csv("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/weather.csv")
View(weather)
head(weather)

# 3. Manakah plot garis berikut yang menunjukkan tren rata-rata kecepatan angin di bandara internasional JFK?
# Menghitung rata-rata kecepatan angin per bulan
jfk_data <- weather %>% filter(origin == "JFK")
jfk_monthly_wind <- jfk_data %>%
  group_by(year, month) %>%
  summarise(avg_wind_speed = mean(wind_speed, na.rm = TRUE))

ggplot(jfk_monthly_wind, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = avg_wind_speed)) +
  geom_line(color = "blue") +
  labs(title = "Tren Rata-rata Kecepatan Angin di Bandara Internasional JFK",
       x = "Tanggal",
       y = "Rata-rata Kecepatan Angin (mph)") +
  theme_minimal()

# 4. Manakah bandara internasional yang memiliki rata-rata kelembapan terendah?
summary(weather$humid)
min_humidity_airport <- aggregate(humid ~ origin, data = weather, FUN = mean)
min_humidity_airport[which.min(min_humidity_airport$humid), ]

# 7. Manakah gambar berikut yang menunjukkan boxplot per bulan dari kecepatan angin di bandara internasional JFK?
jfk_weather <- weather %>% filter(origin == "JFK")

ggplot(jfk_weather, aes(x = factor(month), y = wind_speed)) +
  geom_boxplot() +
  labs(title = "Boxplot Kecepatan Angin per Bulan (JFK)",
       x = "Bulan",
       y = "Kecepatan Angin") +
  theme_minimal()

# 8. Dari data pada bulan Februari di bandara internasional JFK, buat persamaan regresi dari hubungan antara suhu terhadap kelembapan udara. Berdasarkan persamaan regresi dugaan yang diperoleh, jika suhu sebesar 45°F berapakah kelembapan udara?
predicted_humidity <- predict(lm_model, newdata = new_data)
predicted_humidity

# 9. Pada bulan apakah di bandara internasional John F. Kennedy (JFK) memiliki rata-rata suhu terendah?
jfk_data <- weather %>% filter(origin == "JFK")

monthly_avg_temp <- jfk_data %>%
  group_by(month) %>%
  summarize(mean_temp = mean(temp))
# Memperoleh bulan dengan rata-rata suhu terendah
coldest_month <- monthly_avg_temp[which.min(monthly_avg_temp$mean_temp), ]
coldest_month

# 10. Berapakah rata-rata suhu (temp) di bandara internasional Newark Liberty (EWR), John F. Kennedy (JFK): , LaGuardia (LGA) pada bulan Januari?
january_data <- weather %>% filter(month == 1)

# Menghitung rata-rata suhu untuk setiap bandara pada bulan Januari
january_avg_temp <- january_data %>%
  group_by(origin) %>%
  summarize(mean_temp = mean(temp))
january_avg_temp

# 12. Di bandara internasional JFK, pada bulan apakah rata-rata kecepatan angin tertinggi itu terjadi?
# Menghitung rata-rata kecepatan angin untuk setiap bulan di bandara JFK
jfk_monthly_wind <- jfk_data %>%
  group_by(month) %>%
  summarize(mean_wind_speed = mean(wind_speed, na.rm = TRUE))

highest_wind_month <- jfk_monthly_wind %>%
  filter(mean_wind_speed == max(mean_wind_speed))
highest_wind_month

# 14. Pada bulan apakah di bandara internasional John F. Kennedy (JFK) memiliki rata-rata suhu tertinggi?
jfk_monthly_temp <- jfk_data %>%
  group_by(month) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE))

# Mengidentifikasi bulan dengan rata-rata suhu tertinggi
highest_temp_month <- jfk_monthly_temp %>%
  filter(mean_temp == max(mean_temp))
highest_temp_month

# 18.  Manakah gambar berikut yang menunjukkan plot pencar dan garis regresi dari suhu versus kelembapan udara pada bulan Juli di bandara internasional JFK?
jfk_july_data <- filter(weather, origin == "JFK" & month == 7)

ggplot(jfk_july_data, aes(x = temp, y = humid)) +
  geom_point() + # plot pencar
  geom_smooth(method = "lm", se = FALSE) + # garis regresi
  labs(title = "Plot Pencar dan Garis Regresi Suhu vs. Kelembapan Udara (JFK - Juli)",
       x = "Suhu (°F)", y = "Kelembapan Udara (%)")

# 19. Manakah gambar berikut yang menunjukkan densitas suhu per bulan di bandara internasional JFK? 
jfk_data$time_hour <- as.Date(jfk_data$time_hour)

# Ekstrak bulan dari kolom 'time_hour'
jfk_data$month <- strftime(jfk_data$time_hour, "%m")
jfk_data$month <- month.abb[as.numeric(jfk_data$month)]
jfk_data$month.ord <- factor(jfk_data$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",                                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(jfk_data, aes(x = temp, y = month.ord, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [°F]", option = "C") +
  labs(title = 'Temperature Distribution at JFK Airport in 2013') +
  labs(x = "Temperature (°F)") +
  labs(y = "Month")



# Membaca data data_uas_adv1.csv
library(tidyr)
my_data <- read_csv("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/data_uas_adv1.csv")
my_data

# 13. Manakah fungsi di kode R berikut yang dapat digunakan untuk menyusun data dari format long ke format wide?
data_wide <- spread(data_long, key = variable, value = value)
data_wide