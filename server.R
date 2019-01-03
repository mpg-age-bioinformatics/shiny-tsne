.libPaths("/srv/shiny-server/tsne/libs")
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
library(tidyverse)
library(Rtsne)

options(shiny.maxRequestSize=30*1024^2) 

futile.logger::flog.threshold(futile.logger::ERROR, name = "ScatterplotLogger")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # reformat input data
  df <- reactive({
    req(input$uploaded_file)
    inFile <- input$uploaded_file
    filetype <- input$filetype
    
    if (is.null(inFile)){
      return(NULL)
    }
    
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
      D <-read.csv(inFile$datapath, header = input$header, sep = filetype)
    }
    
  })

  output$factors <- renderUI({
    selectInput(inputId = "factors", 
                label = "Select factor", 
                choices = c(c("--select--", "NONE"), names(df())), selected = "--select--")
  })

  plot.data <- reactive({
    D.plot <- df()
    
    D.plot[D.plot == ''] <- NA
    
    D.plot <- na.omit(D.plot)
    
    req( input$factors != "--select--" )
    
    if ( input$factors != "NONE" ){ 
      Labels<-as.vector(D.plot[input$factors])
      D.plot<-D.plot[ , !(names(D.plot) %in% c(input$factors))]
    }

    tsne <- Rtsne( D.plot , dims = input$dims, perplexity=input$perplexity, verbose=FALSE, max_iter = input$max_iter, pca = as.logical(input$pca), check_duplicates = FALSE)

    tsne.vals<-tsne$Y
    colnames(tsne.vals) <- c("x.coord", "y.coord")
    tsne.vals<-data.frame(tsne.vals)
    tsne.vals[, 'color_code'] <- as.factor(1)
    if ( input$factors != "NONE" ){ 
      tsne.vals[, "label"]<-unlist(Labels)
      tsne.vals[, "color_code"] <- as.factor(unlist(Labels)) #tsne.vals[, input$factors]
    }
    tsne.vals
  })

  ranges1 <- reactiveValues(x = NULL, y = NULL)
  ranges2 <- reactiveValues(x = NULL, y = NULL)

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges1$x <- c(brush$xmin, brush$xmax)
      ranges1$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges1$x <- NULL
      ranges1$y <- NULL
    }
  })
  
  observeEvent(input$plot2_dblclick, {
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  
  output$scatterplot1 <- renderPlot({
    D<-plot.data()
    p <- ggplot(D, aes(x=x.coord, y=y.coord, color = color_code))
    p<-p + geom_point(size=input$scex, shape=as.integer(input$pch) ) 
    if(!as.logical(input$showLegend)){
      p<-p+theme(legend.position="none")
    }
    p <- p + ggtitle(input$title) + theme(plot.title = element_text(hjust = 0.5)) 
    p <- p + xlab(input$xlabel) + ylab(input$ylabel)
    if(input$showGrid == TRUE){
      p <- p + theme_bw() 
    } else {
      p <- p + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = input$fontsize.title),
                   axis.text = element_text(colour="black", size = input$fontsize.axis),
                   axis.title = element_text(colour="black", size = input$fontsize.axis),
                   legend.text = element_text(colour="black", size = input$fontsize.legend),
                   legend.title = element_text(colour="black", size = input$fontsize.legend)
    )
    
    ## xlow=layer_scales(p)$x$range$range[1]
    ## xupper=layer_scales(p)$x$range$range[2]
    ## ylow=layer_scales(p)$y$range$range[1]
    ## yupper=layer_scales(p)$y$range$range[2]
    ## if (!is.na(input$lowerx)){
    ##   xlow=input$lowerx
    ## }
    ## if (!is.na(input$upperx)){
    ##   xupper=input$upperx
    ## }
    ## if (!is.na(input$lowery)){
    ##   ylow=input$lowery
    ## }
    ## if (!is.na(input$uppery)){
    ##   yupper=input$uppery
    ## }
    ## p <- p+xlim(xlow, xupper) + ylim(ylow, yupper) 
    p
  })
  
  
  # online plot
  output$scatterplot2 <- renderPlot({
    D<-plot.data()
    p <- ggplot(D, aes(x=x.coord, y=y.coord, color = color_code))

    p <- p +  geom_point(size=input$scex, shape=as.integer(input$pch)) 
    p <- p + ggtitle(input$title) + theme(plot.title = element_text(hjust = 0.5)) 
    p <- p + xlab(input$xlabel) + ylab(input$ylabel)
    
    if(is.null(ranges2$x)){
      p <- p + coord_cartesian(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE)
    } else {
      p <- p + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    }

    if(!as.logical(input$showLegend)){
      p<-p+theme(legend.position="none")
    }
    
    if(input$showGrid == TRUE){
      p <- p + theme_bw() 
    } else {
      p <- p + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = input$fontsize.title),
                   axis.text = element_text(colour="black", size = input$fontsize.axis),
                   axis.title = element_text(colour="black", size = input$fontsize.axis),
                   legend.text = element_text(colour="black", size = input$fontsize.legend),
                   legend.title = element_text(colour="black", size = input$fontsize.legend)
    )
    
    # xlow=layer_scales(p)$x$range$range[1]
    # xupper=layer_scales(p)$x$range$range[2]
    # ylow=layer_scales(p)$y$range$range[1]
    # yupper=layer_scales(p)$y$range$range[2]
    # if (!is.na(input$lowerx)){
    #   xlow=input$lowerx
    # }
    # if (!is.na(input$upperx)){
    #   xupper=input$upperx
    # }
    # if (!is.na(input$lowery)){
    #   ylow=input$lowery
    # }
    # if (!is.na(input$uppery)){
    #   yupper=input$uppery
    # }
    # p <- p+xlim(xlow, xupper) + ylim(ylow, yupper) 
    p
    })
  
  output$info <- renderPrint({
    if(is.null(ranges2$x)){
      brushedPoints(plot.data(), input$plot1_brush)
    } else {
      D = plot.data()
      D = subset(D, x.coord >= ranges2$x[1] & x.coord <= ranges2$x[2])
      D = subset(D, y.coord >= ranges2$y[1] & y.coord <= ranges2$y[2])
      print(D)
    }
  })
  
  output$info2 <- renderPrint({
    res <- nearPoints(plot.data(), input$plot_click, "x.coord", "y.coord")
    if (nrow(res) == 0)
      return()
    res
    })
  
  # to download plot
  output$downloadPlot1 <- downloadHandler(
    
    # specify file name
    filename = function(){
      paste0(input$outfile,".",gitversion(),'.pdf')
    },
    content = function(filename){
      
      # plot
      D<-plot.data()
      p <- ggplot(D, aes(x=x.coord, y=y.coord, color = color_code))
      p<-p + geom_point(size=input$scex, shape=as.integer(input$pch) ) 
      if(!as.logical(input$showLegend)){
        p<-p+theme(legend.position="none")
      }
      p <- p + ggtitle(input$title) + theme(plot.title = element_text(hjust = 0.5)) 
      p <- p + xlab(input$xlabel) + ylab(input$ylabel)
      if(input$showGrid == TRUE){
        p <- p + theme_bw() 
      } else {
        p <- p + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
      p <- p + theme(plot.title = element_text(hjust = 0.5, size = input$fontsize.title),
                     axis.text = element_text(colour="black", size = input$fontsize.axis),
                     axis.title = element_text(colour="black", size = input$fontsize.axis),
                     legend.text = element_text(colour="black", size = input$fontsize.legend),
                     legend.title = element_text(colour="black", size = input$fontsize.legend)
      )
      
      # save plot
      ggsave(filename, p, device = 'pdf')
    }
  )
  
  output$downloadPlot2 <- downloadHandler(
    
    # specify file name
    filename = function(){
      paste0(input$outfile, ".zoom.", gitversion(),'.pdf')
    },
    content = function(filename){
      
      # plot
      D<-plot.data()
      p <- ggplot(D, aes(x=x.coord, y=y.coord, color = color_code))
      
      p <- p +  geom_point(size=input$scex, shape=as.integer(input$pch)) 
      p <- p + ggtitle(input$title) + theme(plot.title = element_text(hjust = 0.5)) 
      p <- p + xlab(input$xlabel) + ylab(input$ylabel)
      
      if(is.null(ranges2$x)){
        p <- p + coord_cartesian(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE)
      } else {
        p <- p + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
      }
      
      if(!as.logical(input$showLegend)){
        p<-p+theme(legend.position="none")
      }
      
      if(input$showGrid == TRUE){
        p <- p + theme_bw() 
      } else {
        p <- p + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
      p <- p + theme(plot.title = element_text(hjust = 0.5, size = input$fontsize.title),
                     axis.text = element_text(colour="black", size = input$fontsize.axis),
                     axis.title = element_text(colour="black", size = input$fontsize.axis),
                     legend.text = element_text(colour="black", size = input$fontsize.legend),
                     legend.title = element_text(colour="black", size = input$fontsize.legend)
      )

      # save plot
      ggsave(filename, p, device = 'pdf')
    }
  )
  
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$outfile,".filtered_table.",gitversion(),".tsv", sep = "")
    },
    content = function(filename) {
      inFile <- input$uploaded_file
      
      if (is.null(inFile))
        return(NULL)
      if(is.null(ranges2$x)){
        D <- brushedPoints(plot.data(), input$plot1_brush)
      } else {
        D = plot.data()
        D = subset(D, x.coord >= ranges2$x[1] & x.coord <= ranges2$x[2])
        D = subset(D, y.coord >= ranges2$y[1] & y.coord <= ranges2$y[2])
        print(D)
      }
      write.table(D, filename, row.names = FALSE, quote = FALSE, sep = '\t')
    }
  )
  
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  }
  )
})

