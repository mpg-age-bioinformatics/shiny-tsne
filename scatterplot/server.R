.libPaths("/srv/shiny-server/scatterplot/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}
library(shiny)
library(xlsx)
library(ggplot2)

futile.logger::flog.threshold(futile.logger::ERROR, name = "ScatterplotLogger")

# Define server logic required to draw a histogram
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
    if (input$columnx == "--select--"){
      updateSelectInput(session, "columnx","Select X-axis", choices = vars)
    }
    if (input$columny == "--select--"){
      updateSelectInput(session, "columny","Select Y-axis", choices = vars)
    }
    req(input$columnx != "--select--", input$columny != "--select--")
    req(input$columnx != input$columny)

    D[D == ''] <- NA
    
    D <-D[,c(input$columnx,input$columny)]
    
    D <- na.omit(D)
    x<-input$columnx
    y<-input$columny
    
    D["x.coord"]<-D$x
    D["y.coord"]<-D$y
    
    return(D)
  })
  
  # online plot
  output$scatterplot <- renderPlot({
    D<-plot.data()
    
    p <- ggplot(D, aes(x=x.coord, y=y.coord)) + theme_bw()
    p <- p +  geom_point(size=input$scex, shape=as.integer(input$pch) ) 
    p <- p + ggtitle(input$title) + theme(plot.title = element_text(hjust = 0.5)) 
    p <- p + xlab(input$xlabel) + ylab(input$ylabel)
    xlow=layer_scales(p)$x$range$range[1]
    xupper=layer_scales(p)$x$range$range[2]
    ylow=layer_scales(p)$y$range$range[1]
    yupper=layer_scales(p)$y$range$range[2]
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
    p <- p+xlim(xlow, xupper) + ylim(ylow, yupper) 
    p
    })
  
  output$info <- renderPrint({
    brushedPoints(plot.data(), input$plot_brush)
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
      plot(D[input$columnx], D[input$columny], main="Scatterplot Example", 
           xlab="x", ylab="y", pch=19)
      # close device
      dev.off()
    }
  )
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  }
  )
})

