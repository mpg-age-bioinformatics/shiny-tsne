.libPaths("/srv/shiny-server/histogram/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}
library(shiny)
library(xlsx)

futile.logger::flog.threshold(futile.logger::ERROR, name = "HistogramLogger")

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
    updateSelectInput(session, "column","Select Column", choices = vars)
    req(input$column)
    
    D[D == ''] <- NA

    plot.data.tmp <- D[[input$column]]
    #class(plot.data.tmp)
    #lapply(plot.data.tmp, function(x) x[!is.na(x)])
    plot.data.tmp<-plot.data.tmp[!is.na(plot.data.tmp)]
    plot.data.tmp<-as.numeric(plot.data.tmp)
    return(plot.data.tmp)
  })
  
  plot.xlim <- reactive({
    xlim=range(plot.data())
    if (!is.na(input$lowerx)) {
      if (!is.na(input$upperx)) {
        xlim=c(input$lowerx,input$upperx)
      }
    }
    return(xlim)
  })
  
  plot.ylim<- reactive({
    ylim=NULL
    if (!is.na(input$lowery)) {
      if (!is.na(input$uppery)) {
        ylim=c(input$lowery,input$uppery)
      }
    }
    return(ylim)
  })
    
  plot.xlabel<-reactive({
    if (input$xlabel == ""){
      xlabel=input$column
    } else {
      xlabel=input$xlabel
    }
    return(xlabel)
  })

  plot.breaks<-reactive({
    if (is.na(input$breaks)){
      breaks="Sturges"
    } else {
      breaks=input$breaks
    }
    return(breaks)
  })
  
  plot.figure<-reactive({
    HT<-hist(plot.data(),
        main=input$title,
        xlab=plot.xlabel(),
        xlim=plot.xlim(),
        ylim=plot.ylim(),
        border="black",
        col="gray",
        las=1, # Rotate the labels on the y axis by adding “las = 1” as an argument. las can be 0, 1, 2 or 3.
        breaks=plot.breaks(), # number of breakpoints
        prob = input$probability,
        lwd = input$linewidth ) 
    
    if (isTRUE(input$density)){ 
      if (isTRUE(input$probability)){
        lines(density(plot.data()),lwd = input$linewidth) 
      }
    }
  })

  # online plot
  output$histogram <- renderPlot({
    plot.figure()
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
      HT<-hist(plot.data(),
        main=input$title,
        xlab=plot.xlabel(),
        xlim=plot.xlim(),
        ylim=plot.ylim(),
        border="black",
        col="gray",
        las=1, # Rotate the labels on the y axis by adding “las = 1” as an argument. las can be 0, 1, 2 or 3.
        breaks=plot.breaks(), # number of breakpoints
        prob = input$probability,
        lwd = input$linewidth ) 
    
      if (isTRUE(input$density)){ 
        if (isTRUE(input$probability)){
          lines(density(plot.data()),lwd = input$linewidth) 
        }
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

