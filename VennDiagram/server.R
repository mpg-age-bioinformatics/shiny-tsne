#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
.libPaths("/srv/shiny-server/VennDiagram/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}

library(shiny)
library(VennDiagram)
library(xlsx)
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # reformat input data
  plot.data <- reactive({
    inFile <- input$file1
    filetype <- input$filetype
    
    if (is.null(inFile))
      return(NULL)
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
    D[D == ''] <- NA
    plot.data.tmp <- as.list(D)
    lapply(plot.data.tmp, function(x) x[!is.na(x)])
    
  })
  
  # reformat input color
  plot.color <- reactive({
    venncolors <- input$venncolors
    color.tmp <- gsub(' ', '', venncolors)
    color.tmp <- unlist(strsplit(venncolors, ','))
  })
  output$testtext <- renderText({
    paste( length(rep(input$linetype, 2)))
  })
  
  # online plot
  output$venn <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    VD <- venn.diagram(plot.data(), filename = NULL, fill = plot.color(), cex = input$num.fontsize,
                       margin = input$marginsize, cat.cex = input$cat.fontsize, 
                       cat.fontface = input$cat.face, fontface = input$cat.face,
                       cat.fontfamily = input$cat.font, fontfamily = input$cat.font, lwd = input$linewidth,
                       lty = rep(as.numeric(input$linetype), length(plot.data)))
    grid.draw(VD)
  })
  
  # table of objects in overlap
  output$overlap <- renderTable({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
    if (input$table){
      OV<- calculate.overlap(plot.data())
      OV <- sapply(OV, as.character)
      n.obs <- sapply(OV, length)
      seq.max <- seq_len(max(n.obs))
      mat <- t(sapply(OV, "[", i = seq.max))
      data.frame(t(mat))
    } else {
      return(NULL)
    }
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

      # create plot
      VD <- venn.diagram(plot.data(), filename = NULL, fill = plot.color(), cex = input$num.fontsize,
                         margin = input$marginsize, cat.cex = input$cat.fontsize, 
                         cat.fontface = input$cat.face, fontface = input$cat.face,
                         cat.fontfamily = input$cat.font, fontfamily = input$cat.font, lwd = input$linewidth,
                         lty = rep(as.numeric(input$linetype), length(plot.data)))
      grid.draw(VD)

      # close device
      dev.off()
    }
  )
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$outfile,".overlap.",gitversion(),".csv", sep = "")
    },
    content = function(file) {
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      if (input$table){
        OV<- calculate.overlap(plot.data())
        OV <- sapply(OV, as.character)
        n.obs <- sapply(OV, length)
        seq.max <- seq_len(max(n.obs))
        mat <- t(sapply(OV, "[", i = seq.max))
        OV <- data.frame(t(mat))
        
        write.csv(OV, file, row.names = FALSE, quote = FALSE, sep = '\t')
      }
    }
  )
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  })
})
