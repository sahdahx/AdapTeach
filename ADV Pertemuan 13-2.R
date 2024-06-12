# DATA IRIS
data(iris)
str(iris)

# berapa jumlah pengamatan untuk setiap spesies bunga iris?
table(iris$Species)
summary(iris)

library(PerformanceAnalytics)
chart.Correlation(iris[,-5], histogram=TRUE, pch=19)

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(size = 3, aes(shape = Species))+
  labs(x = "Sepal length in cm", y = "Sepal width in cm")+
  theme_bw()

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(size = 3, aes(shape = Species)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sepal length in cm", y = "Sepal width in cm") +
  facet_wrap(~ Species)+
  theme_bw()

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(size = 3, aes(shape = Species)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sepal length in cm", y = "Sepal width in cm") +
  facet_grid(Species ~ .)+
  theme_bw()

ggplot(data=iris, aes(x=Species, y=Sepal.Length))+
  geom_boxplot(aes(fill=Species)) +
  labs(y = "Sepal length in cm", title = "Iris boxplot")+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  theme_bw()+
  theme(legend.position = "none")

ggplot(data=iris, aes(x=Sepal.Width, fill=Species))+
  geom_density(stat="density", alpha=I(0.2)) +
  xlab("Sepal Width") + ylab("Density") + ggtitle("Histogram & Density Curve of Sepal Width")+
  theme_bw()
