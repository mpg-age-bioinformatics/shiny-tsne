.libPaths("/srv/shiny-server/histogram/libs")
library(shiny)
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
    class(plot.data.tmp)
    lapply(plot.data.tmp, function(x) x[!is.na(x)])
    plot.data.tmp<-as.numeric(plot.data.tmp)
  })
  
  plot.xlim <- reactive({
    xlim=NULL
    if (!is.na(input$lowerx)) {
      if (!is.na(input$upperx)) {
        xlim=c(input$lowerx,input$upperx)
      }
    }
  })
  
  plot.ylim<- reactive({
    ylim=NULL
    if (!is.na(input$lowery)) {
      if (!is.na(input$uppery)) {
        ylim=c(input$lowery,input$uppery)
      }
    }
  })
    
  plot.xlabel<-reactive({
    if (input$xlabel == ""){
      xlabel=input$column
    } else {
      xlabel=input$xlabel
    }
  })

  plot.breaks<-reactive({
    if (is.na(input$breaks)){
      breaks="Sturges"
    } else {
      breaks=input$breaks
    }
  })
  
  # online plot
  output$histogram <- renderPlot({
    inFile <- input$file1
    req(inFile)
    #data<-plot.data()
    #req<-input$file1
    #req(input$column)
    
    xlim<-plot.xlim()
    if (is.null(xlim))({ xlim=range(plot.data()) })
    
    HT<-hist(plot.data(),
        main=input$title,
        xlab=plot.xlabel(),
        xlim=xlim,
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
  
  
  # to download plot
  output$downloadPlot <- downloadHandler(
    
    # specify file name
    filename = function(){
      'Histogram.pdf'
    },
    content = function(filename){
      # open device
      pdf(filename)
      
      # create plot
      xlim<-plot.xlim()
      if (is.null(xlim))({ xlim=range(plot.data()) })
      
      HT<-hist(plot.data(),
               main=input$title,
               xlab=plot.xlabel(),
               xlim=xlim,
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
})

