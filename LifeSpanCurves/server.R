#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

.libPaths("/srv/shiny-server/LifeSpanCurves/libs")
gitversion <- function(){
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}


library(shiny)
library(tidyverse)
library(survival)
library(xlsx)
library(knitr)
#library(tinytex)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Read file ----
  
  df <- reactive({
    req(input$uploaded_file)
    inFile <- input$uploaded_file
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
      read.xlsx(inFile$datapath, sheetIndex = 1, header = input$header)
    } else {
      read.csv(inFile$datapath, header = input$header, sep = filetype)
    }
  })
  
  # Dynamically generate UI input when data is uploaded ----
  output$day <- renderUI({
    selectInput(inputId = "day", 
                       label = "Select time", 
                       choices = names(df()))
  })
  
  output$deaths <- renderUI({
    selectInput(inputId = "death", 
                       label = "Select deaths (status if long format)", 
                       choices = names(df()))
  })
  
  output$censors <- renderUI({
    selectInput(inputId = "censor",
                       label = "Select censors",
                       choices = names(df()))
  })
  
  output$factors <- renderUI({
    checkboxGroupInput(inputId = "factors", 
                       label = "Select variables", 
                       choices = c(NULL, names(df())))
  })
  
  
  # Select columns and reformat table for survival analysis ----
  # checkbox if already correctly formatted table
  
  df_sel <- reactive({
    req(input$day, input$death)
    if(input$longtable){
      if(is.null(input$factors)){
        D.survival <- df() %>% select(input$day, input$death)
      } else {
        D.survival <- df() %>% select(input$day, input$death, input$factors)
      }
    } else {
      if(is.null(input$factors)){
        df_sel <- df() %>% select(input$day, input$death, input$censor)
      } else {
        df_sel <- df() %>% select(input$day, input$death, input$censor, input$factors)
      }
      
      D = df_sel
      num_var = length(input$factors)
      D.survival <- as.data.frame(matrix(0, nrow = sum(D[,c(input$death, input$censor)]), ncol = 2 + num_var))
      names(D.survival) <- c('day', 'status', input$factors) 
      
      r = 1 # row counter for D.survival
      for(i in 1:nrow(D)){
        if(D[i, input$death] > 0){
          for(event in 1:D[i,input$death]){
            D.survival[r,c(1,2)] <- c(D[i,input$day], 1)
            if(num_var > 0){
              D.survival[r, c(3:ncol(D.survival))] <- as.character(D[i, input$factors])
            }
            r = r+1
          }
        }
        if(D[i, input$censor] > 0){
          for(event in 1:D[i,input$censor]){
            D.survival[r,c(1,2)] <- c(D[i,input$day], 0)
            if(num_var > 0){
              D.survival[r, c(3:ncol(D.survival))] <- as.character(D[i, input$factors])
            }
            r = r+1
          }
        }
      }
      D.survival
    }
  })
  
  plot.color <- reactive({
    colors<- input$colors
    color.legend <- gsub(' ', '', colors)
    color.legend <- unlist(strsplit(color.legend, ','))
    
    if(length(input$factors) > 0){
      df_sel <- df() %>% select(input$factors)
      df_sel[input$factors] <- lapply(df_sel[input$factors], factor)
      categories = levels(df_sel[,1])
    } else {
      categories = '1'
      df_sel = data.frame('no groups' = 1)
      
    }
    if(length(categories) < length(color.legend)){
      color.legend <- color.legend[1:length(categories)]
    }
    if(length(input$factors) == 2){
      color.tmp = rep(color.legend, each = length(levels(df_sel[,2])))
    } else {
      color.tmp = color.legend
    }
    colors <- list(color = color.tmp, category = categories, legend_name = names(df_sel)[1], color.legend = color.legend)
  })
  
  plot.line <- reactive({
    linetype.legend <- as.numeric(input$linetype)
    
    if(length(input$factors) > 1){
      df_sel <- df() %>% select(input$factors)
      df_sel[input$factors] <- lapply(df_sel[input$factors], factor)
      categories = levels(df_sel[,2])
      legend_name = names(df_sel)[2]
    } else if (length(input$factors) == 1){
      df_sel <- df() %>% select(input$factors)
      df_sel[input$factors] <- lapply(df_sel[input$factors], factor)
      categories = levels(df_sel[,1])
      legend_name = names(df_sel)[1]
    } else {
      categories = '1'
      legend_name = ''
    }
    
    if(length(categories) < length(linetype.legend)){
      linetype.legend <- linetype.legend[1:length(categories)]
    }
    if(length(input$factors) == 2){
      linetype.tmp = rep(linetype.legend, times = length(levels(df_sel[,1])))
    } else {
      linetype.tmp = linetype.legend
    }
    linetypes <- list(lty = linetype.tmp, category = categories, legend_name = legend_name, linetype.legend = linetype.legend)
  })
  
  surv.data <- reactive({
    num_var = length(input$factors)

    # plot
    if(num_var == 0){
      fit <- survfit(Surv(day, status) ~ 1, data = df_sel())
    } else {
      rename_variables <- paste('group', 1:num_var, sep = '_')
      plot.data = df_sel()
      names(plot.data) = c('day', 'status', rename_variables)
      # make different models
      if(num_var == 1){
        fit <- survfit(Surv(day, status) ~ group_1, data = plot.data)
      } else if(num_var == 2){
        fit <- survfit(Surv(day, status) ~ group_1 + group_2, data = plot.data)
      } else if (num_var >= 3){
        print(paste("You have selected", num_var, "variables\nPlease select only 2 or less"))
        return(NULL)
      }
    }
    fit
  })
  
  cox <- reactive({
    num_var = length(input$factors)
    
    # plot
    if(num_var == 0){
      fit <- survfit(Surv(day, status) ~ 1, data = df_sel())
    } else {
      rename_variables <- paste('group', 1:num_var, sep = '_')
      plot.data = df_sel()
      names(plot.data) = c('day', 'status', rename_variables)
    
      # make different models
      if(num_var == 1){
           cox <- coxph(Surv(day,status) ~ ., data= plot.data, method="breslow")
      } else if(num_var == 2){
        if(as.logical(input$interaction_term)){
          cox <- coxph(Surv(day,status) ~ . + group_1 * group_2, data= plot.data, method="breslow")
        } else {
          cox <- coxph(Surv(day,status) ~ ., data= plot.data, method="breslow")
        }
      } else if (num_var >= 3){
        print(paste("You have selected", num_var, "variables\nPlease select only 2 or less"))
        return(NULL)
      }
    }
  
  })
  
  output$survPlot <- renderPlot({
    req(input$day, input$death)
    
    # setting the plot titles and styles
    # change here settings here
    main = input$main
    xlab = input$xlab
    ylab = input$ylab
    lwd = input$linewidth
    bxwd = input$boxwidth
    cex.axis = input$axis.size
    cex.lab = input$lab.size
    cex.main = input$main.size
    cex.legend = input$legend.size
    confidence_interval = as.logical(input$conf)
    mark.time = as.logical(input$marks)
    log = input$logaxis
    colors = plot.color()
    linetype = plot.line()
    par(mar = rep(input$margin.size, 4))

    print(linetype$lty)
    plot(surv.data(), conf.int = confidence_interval, col = colors$color, lty = linetype$lty, lwd = lwd, mark.time = mark.time,
        xlab = xlab, ylab = ylab, main = main, log = log,
        cex.axis = cex.axis, cex.lab= cex.lab, cex.main = cex.main)
    box(lwd = bxwd)
    if(length(colors$color.legend) > 1){
      legend('topright', legend = colors$category, col = colors$color.legend, cex = cex.legend, title = colors$legend_name, lty = 1, bty = 'n')
    }
    if(length(linetype$linetype.legend) > 1){
      legend('bottomleft', legend = linetype$category, lty = linetype$linetype.legend, cex = cex.legend, title = linetype$legend_name, col = colors$color.legend[1], bty = 'n')
    }
  })

  # Print data table ----  
  output$survTab <- renderTable({
    if (input$table){
        head(df_sel())
    }
  })

  # print cox proportional hazard model
  output$survStats <- renderPrint({
    req(input$day, input$death)
    print(summary(cox()))
 })
    
    
    
    
  output$downloadPlot <- downloadHandler(
    # specify file name
    filename = function() {
      paste(input$outfile,".plot.",gitversion(),".pdf", sep = "")
    },
    content = function(filename){
      # open device
      pdf(filename, height = input$plot.height, width = input$plot.width)
      
      # create plot # copy from outside
      # copy code from above
      
      # setting the plot titles and styles
      # change here settings here
      main = input$main
      xlab = input$xlab
      ylab = input$ylab
      lwd = input$linewidth
      bxwd = input$boxwidth
      cex.axis = input$axis.size
      cex.lab = input$lab.size
      cex.main = input$main.size
      cex.legend = input$legend.size
      confidence_interval = as.logical(input$conf)
      mark.time = as.logical(input$marks)
      log = input$logaxis
      colors = plot.color()
      linetype = plot.line()
      par(mar = rep(input$margin.size, 4))
      
      print(linetype$lty)
      plot(surv.data(), conf.int = confidence_interval, col = colors$color, lty = linetype$lty, lwd = lwd, mark.time = mark.time,
           xlab = xlab, ylab = ylab, main = main, log = log,
           cex.axis = cex.axis, cex.lab= cex.lab, cex.main = cex.main)
      box(lwd = bxwd)
      if(length(colors$color.legend) > 1){
        legend('topright', legend = colors$category, col = colors$color.legend, cex = cex.legend, title = colors$legend_name, lty = 1, bty = 'n')
      }
      if(length(linetype$linetype.legend) > 1){
        legend('bottomleft', legend = linetype$category, lty = linetype$linetype.legend, cex = cex.legend, title = linetype$legend_name, col = colors$color.legend[1], bty = 'n')
      }
      # close device
      dev.off()
    })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$outfile,".long.",gitversion(),".tsv", sep = "")
    },
    content = function(filename) {
      inFile <- input$uploaded_file
      
      if (is.null(inFile))
        return(NULL)
      if (input$table){
        write.table(df_sel(), filename, row.names = FALSE, quote = FALSE, sep = '\t')
      }
    }
  )
  
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste(input$outfile,".report.", gitversion(),".pdf", sep = "")
    },
    content = function(filename) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list( surv.data = surv.data(), cox = cox(), main = input$main, 
                      xlab = input$xlab, ylab = input$ylab, lwd = input$linewidth,
                      cex.axis = input$axis.size, cex.lab = input$lab.size,
                      cex.main = input$main.size, cex.legend = input$legend.size,
                      confidence_interval = as.logical(input$conf),
                      mark.time = as.logical(input$marks), log = input$logaxis,
                      colors = plot.color(), linetype = plot.line(),
                      margin.size = input$margin.size
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = filename,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  # print app version
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  })
  
})
