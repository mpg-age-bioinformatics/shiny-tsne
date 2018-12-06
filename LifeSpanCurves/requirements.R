# install libraries for LifeSpanCurves

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

if(!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}

if(!require(tinytex)){
  install.packages("tinytex", dependencies = TRUE)
  library(tinytex)
}

if(!require(knitr)){
  install.packages("knitr", dependencies = TRUE)
  library(knitr)
}




