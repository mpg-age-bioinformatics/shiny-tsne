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