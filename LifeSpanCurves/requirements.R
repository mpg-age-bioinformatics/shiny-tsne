# install libraries for VennDiagram

.libPaths("/srv/shiny-server/LifeSpanCurves/libs")

if(!require(shiny)){
  install.packages("shiny", dependencies = TRUE)
  library(shiny)
}

if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}

if(!require(survival)){
  install.packages("survival", dependencies = TRUE)
  library(survival)
}



