install.packages("readr")
library(readr)

# Membaca suatu file txt
my_data <- read_csv("mtcars.txt")
head(my_data)

# Membaca suatu file csv
my_data <- read_csv("mtcars.csv")
head(my_data)
       
# Membaca suatu file dari internet 
my_data <- read_tsv("http://www.sthda.com/upload/boxplot_format.txt")
head(my_data)

# Fungsi problems()dapat digunakan untuk menyelidiki masalah parsing lebih detail.
my_data <- read_csv("mtcars.csv")
problems(my_data)

# Membaca baris dari suatu file
# File demo
my_file <- system.file("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/mtcars.csv", package = "readr")
my_data <- read_lines(my_file)
head(my_data)

# Membaca file keseluruhan
my_file <- system.file("extdata/mtcars.csv", package = "readr")
read_file(my_file)

# MENGEKSPOR DATA DARI R

# 1. Mengekspor data dari R ke file txt|csv|Excel

# Fungsi R dasar untuk mengekspor data
data("Puromycin")
setwd("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/")
write.table(Puromycin, file = "puromycin.txt", sep = "\t", row.names = TRUE, col.names = NA)

# Fungsi write.csv() dan write.csv2() juga dapat digunakan untuk mengekspor data dalam format csv. 
data("Puromycin")
setwd("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/")
write.csv(Puromycin, file = "Puromycin.csv")

# 2. Mengekspor data dari R ke file txt|csv: menggunakan paket readr
install.packages("readr")
library("readr")
# Loading mtcars data
data("mtcars")
library("readr")
# Writing mtcars data to a tsv file
write_tsv(mtcars, path = "mtcars.txt")
# Writing mtcars data to a csv file
write_csv(mtcars, path = "mtcars.csv")

# 3. Mengekspor data dari R ke file Excel (xls|xlsx)

setwd("C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/")
data("USArrests")
library("xlsx")

# Write the first data set in a new workbook
write.xlsx(USArrests, file = "myworkbook.xlsx", sheetName = "USA-ARRESTS", append = FALSE)
# Add a second data set in a new worksheet
data("mtcars")
write.xlsx(mtcars, file = "myworkbook.xlsx", sheetName="MTCARS", append=TRUE)

# 4. Menyimpan data dalam format data R: RDATA dan RDS

# Simpan satu objek dalam suatu file: saveRDS(object, file), readRDS(file)
# Save a single object to a file
saveRDS(mtcars, "mtcars.rds")
# Restore it under a different name
my_data <- readRDS("mtcars.rds")

# Simpan beberapa objek dalam suatu file: save(data1, data2, file), load(file)
# Save multiple objects
save(data1, data2, file = "data.RData")
# To load the data again
load("data.RData")

# Simpan semua workspace: save.image(), load()
# Save your workspace
save.image(file = "my_work_space.RData")
# Load the workspace again
load("my_work_space.RData")

# Membuat tibble baru
install.packages("tibble")
library(tibble)
# Create
friends_data <- data_frame(
    name = c("Nicolas", "Thierry", "Bernard", "Jerome"),
    age = c(27, 25, 29, 26),
    height = c(180, 170, 185, 169),
    married = c(TRUE, FALSE, TRUE, TRUE)
  )
# Print
friends_data
