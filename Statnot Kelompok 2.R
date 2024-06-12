# KELOMPOK 2: UJI KOLMOGOROV-SMIRNOV DAN UJI RUN

# Hipotesis
# H0 = Populasi mengikuti sebaan normal
# H1 = Populasi tidak mengikuti sebaan normal

# Taraf sigifikansi
alpha = 0.05

# Membaca data
Diameter_bunga_matahari <- read.csv("file.csv")
x <- Diameter_bunga_matahari$Diameter.Bunga.Matahari

# Uji normalitas menggunakan Kolmogorov Smirnov
library(nortest)
lillie.test(X)

# Uji Run
# Uji Run adalah suatu urutan lambang-lambang yang sama, yang diikuti serta mengikuti lambang-lambang yang berbeda, atau tidak mengikuti, atau diikuti lambang apapun.
data <- c("Pria", "Wanita", "Pria", "Wanita", "Pria", "Pria", "Pria", "Wanita", "Wanita", "Pria", 
          "Wanita", "Pria", "Wanita", "Pria", "Wanita", "Pria", "Pria", "Pria", "Pria", "Wanita", 
          "Pria", "Wanita", "Pria", "Wanita", "Pria", "Pria", "Wanita", "Wanita", "Wanita", "Pria", 
          "Wanita", "Pria", "Wanita", "Pria", "Wanita", "Pria", "Pria", "Wanita", "Pria", "Pria", 
          "Wanita", "Pria", "Pria", "Pria", "Pria", "Wanita", "Pria", "Wanita", "Pria", "Pria")

# Mengonversi data menjadi biner
binary_data <- ifelse(data == "pria", 1, 0)

# Fungsi untuk meghitung jumlah runs
count_runs <- function(data) {
  runs <- 1
  for (i in 2:length(data)) {
    if (data[i] != data[i-1]) {
      runs <- runs+1
    }
  }
  return(runs)
}

# Menghitung jumlah runs
runs <- count_runs(binary_data)
runs

# Jumlah observasi
n <- length(binary_data)
n

# Jumlah observasi dengan nilai yang sama dengan observasi sebelumnya
n1 <- 30
n1

# Jumlah observasi dengan nilai yang berbeda dari observasi sebelumnya
n2 <- n - n1
n2

# Expected runs
E_R <- (2*n1*n2)/(n1+n2)+1
E_R

# Expected variance
var_R <- (2*n1*n2*(2*n1*n2-n))/((n*n)*(n-1))
var_R

# z-test
z_test <- (runs - E_R)/sqrt(var_R)
z_test

# z-table
alpha <- 0.05

# Mendapatkan nilai dari z-table
z_table_value <- qnorm(1-alpha/2)
z_table_value
