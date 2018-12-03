# install library for heatmap

.libPaths("/srv/shiny-server/heatmap/libs")


if(!require(shiny)){
  install.packages("shiny", dependencies = TRUE)
  library(shiny)
}

if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}


if(!require(gplots)){
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}

if(!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}


# 
# if(!require(heatmaply)){
#   install.packages("heatmaply", dependencies = TRUE)
#   library(heatmaply)
# }
# 
# 
# if(!require(shinyHeatmaply)){
#   install.packages("shinyHeatmaply", dependencies = TRUE)
#   library(shinyHeatmaply)
# }
# 
