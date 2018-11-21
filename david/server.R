.libPaths("/srv/shiny-server/david/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}
library(shiny)
library("RDAVIDWebService")

#     genes_list genes_list_id background_list background_list_id registeredmail gene_ontology_in protein_domains_in pathways_in general_annotations_in functional_categories_in protein_protein_interactions_in literature_in disease_in
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # reformat input data
  target <- reactive({
    inFile <- input$file1
    filetype <- input$filetype
    req(inFile)
    
    filetype_map <- c("xlsx" = 'xlsx',  'tsv' = '\t', 'csv' = ',', 'txt'=" ")
    if(filetype == 'auto'){
      file_extension =  unlist(strsplit(inFile$datapath, '[.]'))[length(unlist(strsplit(inFile$datapath, '[.]')))]
      if(file_extension %in% names(filetype_map)){
        filetype <- filetype_map[file_extension]
        names(filetype) <- NULL
        
      } else {
        print(paste("wrong file format", file_extension))
        return(NULL)
      }
    }
    
    if(filetype == 'xlsx'){
      D <- read.xlsx(inFile$datapath, sheetIndex = 1, header = input$header)
    } else {
      D <- read.csv(inFile$datapath, header = input$header, sep = filetype)
    }
    vars <- names(D)
    updateSelectInput(session, "genes_list","Select Target Genes Column", choices = c(NULL,vars))
    req(input$genes_list)
    
    D[D == ''] <- NA
    
    target.genes <- D[[input$genes_list]]
    class(target.genes)
    lapply(target.genes, function(x) x[!is.na(x)])
    #plot.data.tmp<-as.numeric(plot.data.tmp)
    return(target.genes)
  })

  background <- reactive({
    inFile <- input$file1
    filetype <- input$filetype
    req(inFile)
    
    filetype_map <- c("xlsx" = 'xlsx',  'tsv' = '\t', 'csv' = ',', 'txt'=" ")
    if(filetype == 'auto'){
      file_extension =  unlist(strsplit(inFile$datapath, '[.]'))[length(unlist(strsplit(inFile$datapath, '[.]')))]
      if(file_extension %in% names(filetype_map)){
        filetype <- filetype_map[file_extension]
        names(filetype) <- NULL
        
      } else {
        print(paste("wrong file format", file_extension))
        return(NULL)
      }
    }
    
    if(filetype == 'xlsx'){
      D <- read.xlsx(inFile$datapath, sheetIndex = 1, header = input$header)
    } else {
      D <- read.csv(inFile$datapath, header = input$header, sep = filetype)
    }
    vars <- names(D)
    updateSelectInput(session, "background_list", "Select Background Genes Column", choices = c(NULL,vars)) 
    #req(input$background_list)
    
    D[D == ''] <- NA
    
    if (!is.null(background_list)){
      background.genes <- D[[input$background_list]]
      class(background.genes)
      lapply(background.genes, function(x) x[!is.na(x)])
      #plot.data.tmp<-as.numeric(plot.data.tmp)
    } else {
      background.genes<-NULL
    }
    return(background.genes)
  })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$outfile,".",gitversion(),".csv", sep = "")
    },
    content = function(file) {
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)

      david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")

      categories<-c(input$gene_ontology_in,input$protein_domains_in,input$pathways_in,
      input$general_annotations_in,input$functional_categories_in,
      input$protein_protein_interactions_in,input$literature_in,disease_in)

      lapply(categories, function(x) x[!is.null(x)])

      setAnnotationCategories(david, categories)

      result<-addList(david, target(),
                      idType=input$genes_list_id,
                      listName="target_genes", listType="Gene")

      if (!is.null(background())){
        result<-addList(david, background(),
                idType=input$background_list_id,
                listName="background_genes", listType="Background")
      }

      termCluster<-getClusterReport(david, type="Term")
      getClusterReportFile(david, type="Term", fileName=file)
      
      #if (input$table){
      #  OV<- calculate.overlap(plot.data())
      #  OV <- sapply(OV, as.character)
      #  n.obs <- sapply(OV, length)
      #  seq.max <- seq_len(max(n.obs))
      #  mat <- t(sapply(OV, "[", i = seq.max))
      #  OV <- data.frame(t(mat))
      #  
      #  write.csv(OV, file, row.names = FALSE, quote = FALSE, sep = '\t')
      #}
    }
  )
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  }
  )
})