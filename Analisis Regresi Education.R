# Membaca Data
education<-read.table("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/education.txt", header = TRUE)

# Menampilkan struktur data
str(education)

# Memeriksa data
head(education)

# Membuat model regresi linier berganda
model <- lm(salary ~ ., data = education)

# Menampilkan ringkasan model
summary(model)


# Asumsi regresi linier berganda

# 1. Multikolinearitas
cor_numeric <- cor(education[, c("years_of_experience", "teaching_hours_per_week", "student_teacher_ratio", "school_rating")])
print(cor_numeric)

# 2. Ragam galat diasumsikan konstan darisatu pengamatan ke pengamatan lain (Homoskedastisitas)
residuals <- resid(model)
predicted_values <- fitted(model)
# Memeriksa plot residu vs prediksi
plot(predicted_values, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals vs Fitted", col = "blue")
abline(h = 0, col = "red")  # Garis merah menunjukkan nilai nol

# 3. Galat diasumsikan berdistribusi Normal (Normalitas)
# Menghitung residu dari model regresi
residuals <- residuals(model)
# Memeriksa plot Q-Q dari residu
qqnorm(residuals)
qqline(residuals)

# 4. Galat diasumsikan saling bebas (Autokorelasi)
# Membuat plot residu vs nilai prediksi
plot(model$fitted.values, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals vs Fitted", col = "blue")
abline(h = 0, col = "red")  # Garis merah menunjukkan nilai nol

# 5. Linearitas Model

# 6. Tidak ada outlier