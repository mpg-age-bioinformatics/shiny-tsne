# install libraries for VennDiagram

.libPaths("/srv/shiny-server/VennDiagram/libs")

if(!require(shiny)){
  install.packages("shiny", dependencies = TRUE)
  library(shiny)
}

if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}

if(!require(VennDiagram)){
  install.packages("VennDiagram", dependencies = TRUE)
  library(VennDiagram)
}




