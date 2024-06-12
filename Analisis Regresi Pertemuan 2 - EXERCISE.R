# Nomor 3: create a regression equation based on the data in this link, give interpretation for the regression equation, determine the determination coefficient, correlation, and do the hypothesis testing for correlation.

# Reading data
tugas_data <- read.csv("C:/Users/LENOVO/OneDrive/Documents/Learning R/Semester 2/data_tugas.csv") 
head(tugas_data)

# Create a regression equation
model2<-lm(Height~Weight, data=tugas_data)
# Interpret Regression Equation
summary(model2)

# Estimators b0 and b1
tugas_data$xdif <-tugas_data$Height-mean(tugas_data$Height)
tugas_data$ydif <-tugas_data$Weight-mean(tugas_data$Weight)
tugas_data$crp <-tugas_data$xdif*tugas_data$ydif
tugas_data$xsq <-tugas_data$xdif^2

b1 <-sum(tugas_data$crp)/sum(tugas_data$xsq)
b1

b0 <-mean(tugas_data$Weight) - b1*mean(tugas_data$Height)
b0

# Determination Coefficient 
r_squared <- summary(model2)$r.squared
cat("Koefisien Determinasi (R-squared):", r_squared)

# Correlation Coefficient
r <- sqrt(r_squared)
cat("Correlation Coefficient:", r)

# Hypothesis test for correlation
hypothesis_result <- cor.test(~Weight + Height, data=tugas_data)
print(hypothesis_result)

# UJI HIPOTESIS DAN PENGAMBILAN KESIMPULAN
# HIPOTESIS
p <- hypothesis_result$p.value
cat("p value:", p)
if (p == 0) {
  cat("H0: p=0 (Tidak ada hubungan linier antara Height dan Weight)")
} else {
  cat("H1: p/=0 (Ada hubungan linier antara Height dan Weight)")
}

# TARAF SIGNIFIKANSI
alpha <- 0.05
alpha

# DAERAH KRITIS
# Jika p value lebih kecil dari taraf signifikansi 0,05, maka H0 ditolak dan Ha diterima

# STATISTIK UJI
n <- length(tugas_data$Weight)
df <- n-2

t_hitung <- r*sqrt(n-2)/sqrt(1-r^2)
p_value <- 2 * pt(-abs(t_hitung), df)
cat("t-statistic:", t_hitung)
cat("p-value:", p_value)

# KESIMPULAN
if (p_value > alpha) {
  cat("p-value lebih besar dari taraf signifikansi 0.05 maka uji hipotesis tidak ditolak", alpha, ".\n")
} else {
  cat("p-value kecil dari dari taraf signifikansi 0.05 maka uji hipotesis ditolak", alpha, ".\n")
}


# PPT P2-Anreg: susun tabel ANOVA, kemudian hitung koefisien determinasi dan koefisien korelasi dari data berikut, dan jelaskan dan lakukan pengujian
# Membuat data frame
X <- c(71, 64, 43, 67, 56, 73, 68, 56, 76, 65, 45, 58, 45, 53, 49, 78)
Y <- c(82, 91, 100, 68, 87, 73, 78, 80, 65, 84, 116, 76, 97, 100, 105, 77)
data <- data.frame(X, Y)
data

# Melakukan analisis ANOVA
anova_result <- aov(Y ~ X, data = data)
summary(anova_result)

# Menghitung koefisien determinasi
correlation_matrix <- cor(data)
r_squared <- correlation_matrix[1, 2]^2
cat("Koefisien Determinasi (R-squared):", r_squared, "\n")

# Menghitung koefisien korelasi
cor_coefficient <- cor(data$X, data$Y)
cat("Koefisien Korelasi:", cor_coefficient, "\n")
