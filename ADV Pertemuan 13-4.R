# DATA TEMPERATUR YOGYAKARTA
library(ggplot2)
library(ggridges)
library(gridExtra)
library(viridis)

# Plot densitas temperatur di Yogyakarta tahun 2018
dat <- read.csv(file="C:/Users/LENOVO/OneDrive/Documents/UNY/Semester 2/temp_yogya_2018.csv")
str(dat)

dat$Tanggal <- as.Date(dat$Tanggal, "%d-%m-%Y")
dat$Month <- format(dat$Tanggal, "%b")
dat$Month <- factor(dat$Month, levels = month.abb)

# Create a data frame with Month and Tavg
dat.n <- data.frame(Month = dat$Month, Temp = dat$Tavg)

# Plot densitas
ggplot(dat.n, aes(x = Temp, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [C]", option = "C") +
  labs(title = 'Temperatur di Yogyakarta tahun 2018',
       x = "Temperatur [C]",
       y = "Bulan")
