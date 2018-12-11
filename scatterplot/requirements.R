.libPaths("/srv/shiny-server/scatterplot/libs")

if(!require(futile.logger)){
  install.packages("futile.logger", dependencies = TRUE)
  library(futile.logger)
}

if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}

if(!require(ggplot2)){
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}

if(!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}

quit(save="no")


.libPaths("/srv/shiny-server/scatterplot/libs")
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

setwd("/srv/shiny-server/scatterplot/")
D <- read.xlsx("test_input.xlsx", sheetIndex = 1, header = TRUE)
D_ <-D[,c('xcol','ycol')]
D_ <- na.omit(D_)
y<-'ycol'
x<-'xcol'
Dx<-D_$x
Dy<-D_$y
plot(Dx, Dy, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
x<-'xcol'
y<-'ycol'
ggplot(D_, aes(x=xcol, y=ycol))  + geom_point() + theme_bw() #facet_grid(. ~ cyl) 

p<-ggplot(D_, aes(x=xcol, y=ycol))  + geom_point() + theme_bw()

xlow=layer_scales(p)$x$range$range[1]
xupper=layer_scales(p)$x$range$range[2]
ylow=layer_scales(p)$y$range$range[1]
yupper=layer_scales(p)$y$range$range[2]

p+xlim(xlow, xupper) 


View(D_)
typeof(wt)
wt
Dx
