.libPaths("/srv/shiny-server/david/libs")
library(shiny)

if(!require(RDAVIDWebService)){
  source("http://bioconductor.org/biocLite.R")
  biocLite("RDAVIDWebService")
  library(RDAVIDWebService)
}
