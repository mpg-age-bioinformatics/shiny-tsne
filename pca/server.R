.libPaths("/srv/shiny-server/pca/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}
library(shiny)
library(xlsx)
library(futile.logger)
library(ggbiplot)

futile.logger::flog.threshold(futile.logger::ERROR, name = "PCALogger")

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
    #D.pca <- prcomp(D, center = TRUE,scale. = TRUE)
    return(D)
  })
  
  pca.data <- reactive({
    if (input$scale) {
      D.pca<-prcomp(plot.data(), center = TRUE,scale. = TRUE)
    } else {
      D.pca<-prcomp(plot.data(), center = FALSE, scale. = FALSE)
    }
  })
  
  groups.data<- reactive({
    if (input$ellipse) {
      x<-row.names(plot.data())
      g<-unique(combn(seq_along(x), 2, FUN = function(cm) Biobase::lcPrefixC(x[cm])))
      g<- g[! g %in% c("") ]
      gg<-c()
      for ( n in g ){
        gg<-c( gg, rep(n, length( grep(n,x) ) ) )
      }
    } else {
      gg<-NULL
    }
    return(gg)
  })
    
  output$PCA <- renderPlot({
    pplot<-ggbiplot(pca.data(), var.axes=input$arrows, ellipse=input$ellipse, labels=rownames(plot.data()), choices=c(input$x,input$y), groups= groups.data()) + 
      ggtitle(input$title) + theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5) , aspect.ratio = 1) 
    pplot
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

