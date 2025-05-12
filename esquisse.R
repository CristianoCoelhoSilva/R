library(esquisse)
library(ggplot2)
library(iris)

install.packages(iris)

install.packages("dados")

esquisser()


library(ggplot2)



esquisser(viewer = 'pane')

options("esquisse.viewer" = "browser")

pinguins <- dados::pinguins
iris <- (iris)
d_bg <- iris[, -5]



library(ggplot2)
data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")

mpg_select <- mpg[mpg$manufacturer %in% c("audi", "ford", "honda", "hyundai"), ]

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg_select, aes(displ, cty)) + 
  labs(subtitle="mpg: Displacement vs City Mileage",
       title="Bubble chart")

g + geom_jitter(aes(col=manufacturer, size=hwy)) + 
  geom_smooth(aes(col=manufacturer), method="lm", se=F)
