.libPaths("/srv/shiny-server/tsne/libs")

if(!require(futile.logger)){
  install.packages("futile.logger", dependencies = TRUE)
  library(futile.logger)
}

if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}

if(!require(Rtsne)){
  install.packages("Rtsne", dependencies = TRUE)
  library(Rtsne)
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


.libPaths("/srv/shiny-server/tsne/libs")
setwd("/srv/shiny-server/tsne/")

library(Rtsne)
train<- read.csv("train.csv") 

#Labels<-train$label
#train$label<-as.factor(train$label)

#colors = rainbow(length(unique(train$label)))
#names(colors) = unique(train$label)
row.names(train)<-train[,"label"]
train<-train[ , !(names(train) %in% c("label"))]

tsne <- Rtsne( train[ , !(names(train) %in% c("label"))] , dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
plot(tsne$Y, t='n', main="tsne")
#text(tsne$Y, labels=train$label, col=colors[train$label])



train<- read.csv("train.csv") 

Labels<-as.vector(train["label"])
train<-train[ , !(names(train) %in% c("label"))]
train$label<-as.factor(unlist(Labels)) 

colors = rainbow(length(unique(train$label)))
names(colors) = unique(train$label)
#train<-train[ , !(names(train) %in% c("label"))]

tsne <- Rtsne( train[ , !(names(train) %in% c("label"))] , dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=train$label, col=colors[train$label])

train<- read.csv("train.csv") 
Labels<-as.vector(train["label"])
train<-train[ , !(names(train) %in% c("label"))]
tsne <- Rtsne( train , dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
tsne.vals<-tsne$Y
colnames(tsne.vals) <- c("x.coord", "y.coord")
tsne.vals<-data.frame(tsne.vals)
tsne.vals[, "label"]<- unlist(Labels)
tsne.vals$label[tsne.vals$label==1] <- "John"
Labels<-as.vector(tsne.vals["label"])
View(head(tsne.vals))
tsne.vals[, "color_code"] <- as.factor(unlist(Labels))
p<-ggplot(tsne.vals, aes( x=x.coord, y=y.coord, color=color_code) ) + geom_point(size=1, shape=as.integer(19) )# +theme(legend.position="none")
p
colors <- "black"
colors <- gsub(' ', '', colors)
colors <- unlist(strsplit(colors, ','))
colfunc <- colorRampPalette(colors)
colfunc
#req( input$factors != "--select--" )
#if ( input$factors == "NONE" ){
#  colfunc(1)
  #print("black")
#} else {
levels(tsne.vals[, 'color_code'])
length(levels(tsne.vals[, 'color_code']))
color<-colfunc(2)#colfunc(length(levels(tsne.vals[, 'color_code'])))

color

p <- p + scale_color_manual(values = color, name = 'label')
p
p + scale_color_manual(values = color, guide = as.logical(FALSE) )
p
#print("multiple")
View(color)


p <- p + scale_color_gradient(low=color[1], high=color[2], name = "label")

train$label<-as.factor(unlist(Labels)) 

train$label<-as.factor(unlist(Labels)) 
tsne.vals<-tsne$Y
colnames(tsne.vals) <- c("x.coord", "y.coord")
tsne.vals<-data.frame(tsne.vals)

tsne.vals[, "color_code"] <- as.factor(unlist(Labels))


View(tsne$Y)



train$label<-unlist(Labels)

train[, "label"]<-unlist(Labels)


