.libPaths("/srv/shiny-server/histogram/libs")

if(!require(futile.logger)){
  install.packages("futile.logger", dependencies = TRUE)
  library(futile.logger)
}
  
if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}