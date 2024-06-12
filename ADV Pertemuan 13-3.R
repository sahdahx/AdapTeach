# DATA MTCARS
data("mtcars")
dfm <- mtcars
dfm$cyl <- factor(dfm$cyl)
dfm$name <- rownames(dfm)
head(dfm[, c("name", "wt", "mpg", "cyl")])

upper.panel<-function(x, y){
  points(x,y, pch=1)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt, col = "blue")
}

pairs(mtcars[,c("mpg", "disp", "hp", "drat", "wt", "qsec")], lower.panel = NULL,
        upper.panel = upper.panel)

install.packages("psych")
library(psych)
pairs.panels(mtcars[,c("mpg", "disp", "hp", "drat", "wt", "qsec")],
               method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE, # show density plots
               ellipses = FALSE # doesn't show correlation ellipses
)

# plot densitas
install.packages("ggpubr")
library(ggpubr)
p <- ggdensity(mtcars, x = "mpg", fill = "cyl",
                 palette = "jco",
                 ggtheme = theme_light(), legend = "top")
p

# Facet dengan satu variabel pengelompokkan “am” secara horisontal
facet(p, facet.by = "am")

# Facet dengan satu variabel pengelompokkan “am” secara vertikal
mtcars$am <- factor(mtcars$am)
facet(p, facet.by = "am", ncol = 1)

# Facet dengan dua variabel pengelompokkan (“am”,”cyl”)
facet(p, facet.by = c("am", "cyl"), short.panel.labs = FALSE)

# Memodifikasi penampilan label panel
# Membuat plot dasar
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(x = "Weight", y = "Miles per Gallon", title = "MPG vs Weight by Transmission and Cylinders")

# Plot pencar
mtcars$name <- rownames(mtcars)
ggscatter(mtcars, x = "wt", y = "mpg",
            color = "cyl", palette = "jco",
            label = "name", repel = TRUE)

ggscatter(mtcars, x = "wt", y = "mpg",
          color = "cyl", palette = "jco",
          shape = "cyl",
          ellipse = TRUE)

p <- ggboxplot(mtcars, x = "cyl",
               y = "mpg",
               combine = TRUE,
               color = "cyl", palette = "jco",
               ylab = "Miles/(US) gallon)",
               xlab = "Number of cylinders (cyl)",
               add = "jitter", # Add jittered points
               add.params = list(size = 0.1, jitter = 0.2), # Point size and the amount of jittering
               label = "name",
               label.select = list(top.up = 2, top.down = 2),
               font.label = list(size = 7, face = "italic", color = "cyl"), # label font
               repel = TRUE # Avoid label text overplotting
)
facet(p,facet.by="am", panel.labs= list(am = c("Automatic", "Manual")))

# Main plot
install.packages("cowplot")
library(ggpubr)
library(cowplot)
pmain <- ggplot(mtcars, aes(x = wt, y = mpg, color = cyl))+
  geom_point(size = 3)+
  ggpubr::color_palette("jco") +
  labs (x = "Weight (1000) lbs", y = "Miles/(US) gallon")
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data = mtcars, aes(x = wt, fill = cyl),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette("jco")
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data = mtcars, aes(x = mpg, fill = cyl),
               alpha = 0.7, size = 0.2)+
  coord_flip()+
  ggpubr::fill_palette("jco")
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)

# Plot pencar dengan garis regresi dan interval kepercayaan serta koefisien korelasi dan p-value
ggscatter(mtcars, x = "wt", y = "mpg", size = 0.3,
          color = "cyl", palette = "jco",
          facet.by = "cyl", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor(aes(color = cyl), method = "pearson", label.y = 6)
