.libPaths("/srv/shiny-server/dendogram/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}
library(shiny)
library(xlsx)
library(dendextend)
library(circlize)

futile.logger::flog.threshold(futile.logger::ERROR, name = "DendogramLogger")

# Define server logic required to draw a dendogram
shinyServer(function(input, output, session) {
  
  # reformat input data
  plot.data <- reactive({
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
    if (input$column == ""){
      updateSelectInput(session, "column","Select labels column", 
                        choices = c("--select--","row.names", vars), 
                        selected = "--select--")
    }
    req(input$column != "--select--", input$column != "")

    D[D == ''] <- NA
    D <- na.omit(D)
    
    if ( ! input$column %in% c("--select--","row.names") ) {
      row.names(D)<-D[,input$column]
      D<-D[ , !(names(D) %in% c(input$column))]
    }
    dd <- dist(D, method = input$distance )
    hc <- hclust(dd, method = input$algor )
    hcd <- as.dendrogram(hc)
    
    return(hcd)
  })
  
  output$dendogram <- renderPlot({
    dend<-plot.data()
    if (input$clusters > 1 ) {
      if (input$colors == 'rainbow'){
        dcolor<-rainbow
      } else {
        dcolor <- input$colors
        dcolor <- gsub(' ', '', dcolor)
        dcolor <- unlist(strsplit(dcolor, ','))
      }
      dend <- color_branches(dend, k= input$clusters , col=dcolor )
      dend <- color_labels(dend,k=input$clusters, col=dcolor)
    }
    
    if (input$circular) {
      if (is.null(input$trackheight)) {
        output$trackheight <- renderUI({
          sliderInput('trackheight', 'Select track height (circular only)', min = 0.1, max = 0.9, value = 0.8, step = 0.01)        
        })
      }
      req(input$trackheight)
      circlize_dendrogram( dend , dend_track_height= input$trackheight) 
    } else {
      plot(dend, main = input$title , hang = -1 , cex.main=input$textsize,
           cex.lab=input$textsize, cex.axis=input$textsize, nodePar=list(lab.cex = input$labelssize, pch = c(NA) ) )
    }
  })
  
  # to download plot
  output$downloadPlot <- downloadHandler(
    
    # specify file name
    filename = function(){
      paste0(input$outfile,".",gitversion(),'.pdf')
    },
    content = function(filename){
      dend<-plot.data()
      if (input$clusters > 1 ) {
        if (input$colors == 'rainbow'){
          dcolor<-rainbow
        } else {
          dcolor <- input$colors
          dcolor <- gsub(' ', '', dcolor)
          dcolor <- unlist(strsplit(dcolor, ','))
        }
        dend <- color_branches(dend, k= input$clusters , col=dcolor )
        dend <- color_labels(dend,k=input$clusters, col=dcolor)
      }
      
      # open device
      pdf(filename)
      # plot
      if (input$circular) { 
        circlize_dendrogram( dend , dend_track_height = 0.6) 
      }
      else {
        plot(dend, main = input$title , hang = -1 , cex.main=input$textsize,
             cex.lab=input$textsize, cex.axis=input$textsize, nodePar=list(lab.cex = input$labelssize, pch = c(NA) ) )
      }
      # close device
      dev.off()
    }
  )
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
    }
  )
})

