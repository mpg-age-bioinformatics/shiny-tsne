.libPaths("/srv/shiny-server/mds/libs")

if(!require(futile.logger)){
  install.packages("futile.logger", dependencies = TRUE)
  library(futile.logger)
}

if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}

if(!require(magrittr)){
  install.packages("magrittr", dependencies = TRUE)
  library(magrittr)
}

if(!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}

if(!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE)
  library(ggpubr)
}

#if(!require(devtools)){
#  install.packages("devtools", dependencies = TRUE)
#  library(devtools)
#}

#if(!require(ggbiplot)){
#  library(devtools)
#  install_github("vqv/ggbiplot")
#  library(ggbiplot)
#}

#if(!require(ggplot2)){
#  install.packages("ggplot2", dependencies = TRUE)
#  library(ggplot2)
#}

#if(!require(tidyverse)){
#  install.packages("tidyverse", dependencies = TRUE)
#  library(tidyverse)
#}





quit(save="no")


.libPaths("/srv/shiny-server/mds/libs")

### 

tdf<-read.csv("/srv/shiny-server/dendogram/heatmap_example.csv", row.names = "gene")
tdf[tdf == ''] <- NA
tdf<-na.omit(tdf)
View(tdf)
tdf<-t(tdf)


# Load required packages
library(magrittr)
library(dplyr)
library(ggpubr)


# test data
data("swiss")
View(swiss)

# Cmpute MDS
mds <- swiss %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)

# own data
# Cmpute MDS
mds <- tdf %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(tdf),
          size = 1,
          repel = TRUE)

# K-means clustering
clust <- kmeans(mds, 2)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(tdf),
          color = "groups",
          palette = "jco",
          size = 4, 
          ellipse = FALSE,
          ellipse.type = "confidence",
          ellipse.level = 0.999, 
          repel = TRUE)


source("https://bioconductor.org/biocLite.R")
biocLite("Biobase")
x<-row.names(tdf)
#x<-c("wildtype_rep1" ,"wildtype_rep2", "wildtype_rep3", "wildtype_rep4", "wildtype_rep5",
#     "mut_rep1" ,"mut_rep2", "mut_rep3", "mut_rep4", "mut_rep5",
#     "treated_rep1",  "treated_rep2",  "treated_rep3" , "treated_rep4",  "treated_rep5")

g<-unique(combn(seq_along(x), 5, FUN = function(cm) Biobase::lcPrefixC(x[cm])))
#g
x
g<- g[! g %in% c("") ]
g
g<- c(rep("wt", 5), rep("treated",5))

regmatches(".wildtype_rep", x,3)

sub("wildtype_rep", "", x)

a<-grep("wildtype_rep",x)
length(a)
gg<-c()
for ( n in g ){
  gg<-c( gg, rep(n, length( grep(n,x) ) ) )
}
gg

mtcars.pca <- prcomp(tdf, center = TRUE,scale. = TRUE)
summary(mtcars.pca)
str(mtcars.pca)
p<-ggbiplot(mtcars.pca)
layer_scales(p)$x$range$range[1]



# remove arrows
options(repr.plot.width=10, repr.plot.height=8)
ggbiplot(mtcars.pca,var.axes=FALSE ) #+  geom_point() + coord_equal()
# samples names
ggbiplot(mtcars.pca, var.axes=FALSE , labels=row.names(tdf), ellipse=TRUE)
# groups
ggbiplot(mtcars.pca, var.axes=FALSE , labels=row.names(tdf), ellipse=TRUE, groups=g) #+ geom_point(width = 3, height = 3)


#mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(mtcars.pca, var.axes=FALSE , labels=rownames(mtcars),groups=mtcars.country)
# ellipse on groups
ggbiplot(mtcars.pca, var.axes=FALSE, ellipse=TRUE, labels=rownames(mtcars), groups=mtcars.country)
# different components
ggbiplot(mtcars.pca, var.axes=FALSE, ellipse=TRUE, labels=rownames(tdf), groups=gg, choices=c(1,2)) + 
  ggtitle("PCA of mtcars dataset") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5), aspect.ratio = 1)
  #theme_minimal()#+
  #theme(legend.position = "bottom")
scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue")) +
  
  # xlow=layer_scales(p)$x$range$range[1]
  # xupper=layer_scales(p)$x$range$range[2]
  # ylow=layer_scales(p)$y$range$range[1]
  # yupper=layer_scales(p)$y$range$range[2]
  # if (!is.na(input$lowerx)){
  #   xlow=input$lowerx
  # }
  # if (!is.na(input$upperx)){
  #   xupper=input$upperx
  # }
# if (!is.na(input$lowery)){
#   ylow=input$lowery
# }
# if (!is.na(input$uppery)){
#   yupper=input$uppery
# }
# p <- p+xlim(xlow, xupper) + ylim(ylow, yupper) 
  
# scaling - not really sure on what it's doing or not
ggbiplot(mtcars.pca, var.axes=FALSE, ellipse=TRUE, labels=rownames(mtcars), groups=mtcars.country, choices=c(3,4), obs.scale = 0.5, var.scale = 4) + 
  


#ggbiplot(mtcars.pca, labels=rownames(mtcars))
#mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
#ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)
#ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),   labels=rownames(mtcars), groups=mtcars.country)
#ggbiplot(mtcars.pca,ellipse=TRUE,circle=TRUE, labels=rownames(mtcars), groups=mtcars.country)
#ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country)
#ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=rownames(mtcars), groups=mtcars.country)


View(mtcars[,c(1:7,10,11)])


ellipse = FALSE
ellipse.prob = 0.68
labels.size = 3, 
alpha = 1,
varname.size = 3,
varname.adjust = 1.5
varname.abbrev = FALSE


my.size=4
my.vector=c()
for ( i in seq(1,my.size,1) ){
  my.vector<-c(my.vector,paste0("Dim",i) ) 
}

my.vector

