.libPaths("/srv/shiny-server/dendogram/libs")

if(!require(futile.logger)){
  install.packages("futile.logger", dependencies = TRUE)
  library(futile.logger)
}

if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}

if(!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}

if(!require(DT)){
  install.packages("DT", dependencies = TRUE)
  library(DT)
}

quit(save="no")

setwd("/srv/shiny-server/")
#setwd("/srv/shiny-server/dendogram")
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
D <- read.csv("dendogram/heatmap_example.csv", header = TRUE, sep = ",")
View(D)
ns<-"gene"
row.names(D)<-D[,ns]
D<-D[ , !(names(D) %in% c('gene'))]
#scale()
dd <- dist(D, method = "euclidean")
hc <- hclust(dd, method = "ward.D2")


library("ggplot2")
library("ggdendro")
library(dendextend)
library(circlize)

dend <- as.dendrogram(hc)

num_clades <- 5

dend <- dend %>% 
  color_branches(k=num_clades, col=rainbow) %>% 
  color_labels(k=num_clades, col=rainbow)
par(mar = rep(0, 4))
circlize_dendrogram(dend, dend_track_height = 0.8) 



# Customized colors # rainbow also works here
dend %>% set("labels_col", value = c("red", "blue"), k=2) %>% 
  set("branches_k_color", value = c("red", "blue"), k = 2) %>% 
  plot(main = "Customized colors")

# alternative to expansion
#dend_<-set(dend, "labels_col", value = c("green", "blue"), k=2)
#plot(dend_)


#######################################################
hcd <- as.dendrogram(hc)

#dend <- D %>% scale %>% dist %>% 
#  hclust %>% as.dendrogram %>%
#  set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
#  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
#  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))


#nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
#                cex = 0.7, col = "blue")
#plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
#plot(hcd,  xlab = "Height", nodePar = nodePar, 
#     edgePar = list(col = 1:3, lwd = 2:1))


#colors = c("red", "blue", "green", "black")
#clus4 = cutree(hc, 4)
#plot(as.phylo(hc), type = "fan", tip.color = colors[clus4], branches.color=colors[clus4],
#     label.offset = 1, cex = 0.7)



plot(hc)
plot(hc, hang = -1, cex = 0.6)

plot(hcd, type = "rectangle", ylab = "Height")


plot(hcd, type = "triangle", ylab = "Height")
plot(hcd, xlim = c(1, 20), ylim = c(1,8))

install.packages("ape")
library("ape")

plot(as.phylo(hc), type = "fan")
#######################################################

#install.packages("ggdendro")
#install.packages('dendextend')
install.packages("circlize")





# Build dendrogram object from hclust results
dend <- as.dendrogram(hc)
# Extract the data (for rectangular lines)
# Type can be "rectangle" or "triangle"
dend_data <- dendro_data(dend, type = "rectangle")
# What contains dend_data
names(dend_data)
ggdendrogram(hc)


dend %>% set("labels_col", c("red", "blue")) %>% # change color
  set("labels_cex", 2) %>% # Change size
  plot(main = "Change the color \nand size") # plot


# Customized colors
dend %>% set("labels_col", value = c("red", "blue"), k=2) %>% set("branches_k_color", 
             value = c("red", "blue"), k = 2) %>% 
  plot(main = "Customized colors")

ggd1 <- as.ggdend(dend)
ggplot(ggd1, labels = FALSE) + scale_y_reverse(expand = c(0.2, 0)) +coord_polar(theta="x")
