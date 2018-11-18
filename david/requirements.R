.libPaths("/srv/shiny-server/david/libs")
library(shiny)

if(!require(RDAVIDWebService)){
  source("http://bioconductor.org/biocLite.R")
  biocLite("RDAVIDWebService")
  library(RDAVIDWebService)
}

quit(save = "no", status = 0)

### development area

.libPaths("/srv/shiny-server/david/libs")
library("RDAVIDWebService")
david<-DAVIDWebService$new(email="Jorge.Boucas@age.mpg.de",url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
data(demoList1)

setAnnotationCategories(david, c("GOTERM_BP_ALL","GOTERM_MF_ALL", "GOTERM_CC_ALL"))

result<-addList(david, demoList1,
                idType="AFFYMETRIX_3PRIME_IVT_ID",
                listName="demoList1", listType="Gene")

result

help(addList)
