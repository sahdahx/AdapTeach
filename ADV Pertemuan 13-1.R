# DATA COVID
library(readr)

c19 <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
head(c19)
c19.IND <- subset(c19, location == "Indonesia")
str(c19.IND)

# memilih variabel
d1 <- c19.IND[, c("total_cases", "total_deaths", "new_cases")]
head(d1)
nrow(d1)

# cek data memuat NA
sapply(X = d1, FUN = function(x) sum(is.na(x)))

# menghapus data NA
d1.1 <- d1[complete.cases(d1),]
nrow(d1.1)

# cek data memuat NA
sapply(X = d1.1, FUN = function(x) sum(is.na(x)))

install.packages("Hmisc")
library(Hmisc)
rescor <- rcorr(as.matrix(d1.1))
rescor

# Visualisasi
install.packages("PerformanceAnalytics")
install.packages("corrplot")
install.packages("lubridate")
library(PerformanceAnalytics)
library(corrplot)
library(lubridate)
chart.Correlation(d1.1, histogram=TRUE, pch=19)

corrplot(cor(d1.1), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# mendefinisikan variabel untuk tahun dan bulan
c19.IND$Yr_Month <- format(as.Date(c19.IND$date), "%Y-%m")

#data sampai bulan Mei 2021
d2 <- subset(c19.IND, Yr_Month <= "2024-04-01")

# Plot the data
# geom_col
ggplot(d2, aes(x = Yr_Month, y = total_cases)) +
  geom_col(fill = "pink") +
  labs(x = "", y = "Total cases", title = "Monthly COVID-19 Total Cases in Indonesia: April 2020 - April 2024") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# geom_point
p <- ggplot(data = d2, aes(x = total_cases, y = total_deaths)) +
  geom_point()+
  theme_bw()
p + facet_wrap(~Yr_Month)

# geom_boxplot
ggplot(data = d2, aes(x = total_cases, y = Yr_Month)) +
  geom_boxplot(color="black", fill="white")+coord_flip()+
  labs(y = "", x = "Total cases")+
  theme_bw()

# geom_line
d2$date <- as.Date(d2$date)
ggplot(data = d2, aes(x=date, y= total_cases)) +
  geom_line(color="#DC143C")+
  labs(x ="", y = "Total cases", title = "Daily COVID 19 total cases in Indonesia: April 2020 - May 2021")+
  theme_bw()

# geom_point
c19cont <- subset(c19, continent != "")
ggplot(data = c19cont, aes(x= human_development_index, y=life_expectancy , size = population, color = continent)) +
  geom_point()+
  labs(x = "Human development index", y = "Life expectancy")+
  theme_bw()

