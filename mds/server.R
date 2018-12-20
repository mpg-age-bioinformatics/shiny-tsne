.libPaths("/srv/shiny-server/mds/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}

library(futile.logger)
library(xlsx)
library(magrittr)
library(dplyr)
library(ggpubr)

futile.logger::flog.threshold(futile.logger::ERROR, name = "MDSLogger")

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
    D<-t(D)
    return(D)
  })
  
  dimension.names<-reactive({
    dnames<-c()
    for ( i in seq(1,input$dimension,1) ){
      dnames<-c(dnames,paste0("Dim",i) ) 
    }
    updateSelectInput(session, "x", "X-axis dimension", 
                      choices = dnames, 
                      selected = dnames[1])
    updateSelectInput(session, "y", "Y-axis dimension", 
                      choices = dnames, 
                      selected = dnames[2])
    return(dnames)    
  })
  
  mds.data <- reactive({
    D.mds<- plot.data()
    D.mds<- D.mds %>%
      dist(method=input$method) %>%          
      cmdscale(k=input$dimension) %>%
      as_tibble()
    
    req( input$x %in% dimension.names(), input$y %in% dimension.names())
    
    colnames(D.mds) <- dimension.names()
    if (input$cluster > 0 ) {
      clust <- kmeans(D.mds, input$cluster)$cluster %>%
        as.factor()
      D.mds <- D.mds %>%
        mutate(groups = clust)
    }
    return(D.mds)
  })
  
  data.names<-reactive({
    if (input$names) {
      sample.names<-rownames(plot.data())
    } else {
      sample.names<-NULL
    }
    return(sample.names)
  })
  
  output$mds <- renderPlot({
    if (input$cluster > 1 ) {
      pplot<-ggscatter(mds.data(), x = input$x, y = input$y, 
                       label = data.names(),
                       color = "groups",
                       palette = "jco",
                       size = input$symbols.size, 
                       ellipse = input$ellipse,
                       ellipse.type = "confidence",
                       ellipse.level = input$ellipse.prob,
                       title=input$title,
                       repel = TRUE)
      
    } else {
      pplot<-ggscatter(mds.data(), x = input$x, y = input$y, 
                       label = data.names(),
                       size =  input$symbols.size,
                       title=input$title,
                       repel = TRUE)
    }
    
    xlow=layer_scales(pplot)$x$range$range[1]
    xupper=layer_scales(pplot)$x$range$range[2]
    ylow=layer_scales(pplot)$y$range$range[1]
    yupper=layer_scales(pplot)$y$range$range[2]
    if (!is.na(input$lowerx)){
     xlow=input$lowerx
    }
    if (!is.na(input$upperx)){
     xupper=input$upperx
    }
    if (!is.na(input$lowery)){
     ylow=input$lowery
    }
    if (!is.na(input$uppery)){
     yupper=input$uppery
    }
    pplot <- pplot+xlim(xlow, xupper) + ylim(ylow, yupper) 
    pplot
  })
  

  # to download plot
  output$downloadPlot <- downloadHandler(
    
    # specify file name
    filename = function(){
      paste0(input$outfile,".",gitversion(),'.pdf')
    },
    content = function(filename){
      # open device
      pdf(filename)
      # plot
      if (input$cluster > 1 ) {
        pplot<-ggscatter(mds.data(), x = input$x, y = input$y, 
                         label = data.names(),
                         color = "groups",
                         palette = "jco",
                         size = input$symbols.size, 
                         ellipse = input$ellipse,
                         ellipse.type = "confidence",
                         ellipse.level = input$ellipse.prob,
                         title=input$title,
                         repel = TRUE)
        
      } else {
        pplot<-ggscatter(mds.data(), x = input$x, y = input$y, 
                         label = data.names(),
                         size =  input$symbols.size,
                         title=input$title,
                         repel = TRUE)
      }
      
      xlow=layer_scales(pplot)$x$range$range[1]
      xupper=layer_scales(pplot)$x$range$range[2]
      ylow=layer_scales(pplot)$y$range$range[1]
      yupper=layer_scales(pplot)$y$range$range[2]
      if (!is.na(input$lowerx)){
        xlow=input$lowerx
      }
      if (!is.na(input$upperx)){
        xupper=input$upperx
      }
      if (!is.na(input$lowery)){
        ylow=input$lowery
      }
      if (!is.na(input$uppery)){
        yupper=input$uppery
      }
      pplot <- pplot+xlim(xlow, xupper) + ylim(ylow, yupper) 
      pplot
      # close device
      dev.off()
    }
  )
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
    }
  )
})

