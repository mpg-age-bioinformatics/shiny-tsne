#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

.libPaths("/srv/shiny-server/heatmap/libs")

gitversion <- function(){
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}


library(shiny)
library(xlsx)
library(gplots)

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
  
  output$labels <- renderUI({
    selectInput(inputId = "labels", 
                label = "Select label column", 
                choices = c("row.names", names(df())))
  })
  
  heatmap.data <- reactive({
    req(input$uploaded_file, input$labels)
    
    if(input$labels == "row.names"){
      row_labels <- row.names(df()) 
      D.heatmap <- as.matrix(df())
    } else {
      D.heatmap <- df()
      row_labels = D.heatmap[,input$labels]
      D.heatmap <- as.matrix(D.heatmap[, -1])
    }
    
    if(input$log_transform == 'log2'){
      D.heatmap <- log2(D.heatmap)
    } else if(input$log_transform == "log10"){
      D.heatmap <- log10(D.heatmap) + 1 
    }
    heatmap.data <- list(D = D.heatmap, labels = row_labels)
  })
  
  plot.color <- reactive({
    colors <- input$color
    colors <- gsub(' ', '', colors)
    colors <- unlist(strsplit(colors, ','))
    colfunc <- colorRampPalette(colors)
    colfunc(input$num_col)
  })
  
  
  plot.heatmap <- reactive({
    req(input$uploaded_file, input$labels)
    
    if(input$dendro == 'both'){
      Rowv = TRUE
      Colv = TRUE
    } else if (input$dendro == "row"){
      Rowv = TRUE
      Colv = FALSE
    } else if (input$dendro == 'col'){
      Rowv = FALSE
      Colv = TRUE
    } else {
      Rowv = FALSE
      Colv = FALSE
    }
    # name for color key
    
    heatmap.data = heatmap.data()
    
    HM <- heatmap.2(as.matrix(heatmap.data$D),labRow = heatmap.data$labels, trace = 'none', col = plot.color(),
                    margins = c(input$mar_bottom, input$mar_right), Rowv = Rowv, Colv = Colv, na.color = input$na.color,
                    dendrogram = input$dendro, key = input$color_key, 
                    distfun = function(x) dist(x, method = input$dist), 
                    hclustfun = function(x) hclust(x, method = input$clust), scale = input$heat_scale,
                    cexRow = input$cex_lab, cexCol = input$cex_lab)
  })
  
  
  observe({
    output$heatmap <- renderPlot({
      req(input$uploaded_file, input$labels)

      if(input$dendro == 'both'){
        Rowv = TRUE
        Colv = TRUE
      } else if (input$dendro == "row"){
        Rowv = TRUE
        Colv = FALSE
      } else if (input$dendro == 'col'){
        Rowv = FALSE
        Colv = TRUE
      } else {
        Rowv = FALSE
        Colv = FALSE
      }
      # name for color key

      heatmap.data = heatmap.data()
      
      HM <- heatmap.2(as.matrix(heatmap.data$D),labRow = heatmap.data$labels, trace = 'none', col = plot.color(),
              margins = c(input$mar_bottom, input$mar_right), Rowv = Rowv, Colv = Colv, na.color = input$na.color,
              dendrogram = input$dendro, key = input$color_key, 
              distfun = function(x) dist(x, method = input$dist), 
              hclustfun = function(x) hclust(x, method = input$clust), scale = input$heat_scale,
              cexRow = input$cex_lab, cexCol = input$cex_lab)

      colors2 <- input$color2
      colors2 <- gsub(' ', '', colors2)
      colors2 <- unlist(strsplit(colors2, ','))
      
      if(input$num_clust > nrow(heatmap.data$D)){
        num_clust = nrow(heatmap.data$D)
      } else {
        num_clust = input$num_clust
      } 
      
      tree1 = cutree(hclust(dist(as.matrix(heatmap.data$D), method = input$dist), method = input$clust), num_clust)
      nofclust.height <-  length(unique(as.vector(tree1)));
      selcol2 <- colorRampPalette(colors2)
      clustcol.height = selcol2(nofclust.height)
    
      HM <- heatmap.2(as.matrix(heatmap.data$D),labRow = heatmap.data$labels, trace = 'none', col = plot.color(),
                      margins = c(input$mar_bottom, input$mar_right), Rowv = Rowv, Colv = Colv, na.color = input$na.color,
                      dendrogram = input$dendro, key = input$color_key, 
                      distfun = function(x) dist(x, method = input$dist), 
                      hclustfun = function(x) hclust(x, method = input$clust), scale = input$heat_scale,
                      cexRow = input$cex_lab, cexCol = input$cex_lab, RowSideColors = clustcol.height[tree1])
    }, height = input$plot_height, width = input$plot_width)
  })
  
  
  output$clusterMap <- renderTable({
    if (input$table){
      D <- plot.heatmap()
      heatmap.data = heatmap.data()
      
      if(input$num_clust > nrow(heatmap.data$D)){
        num_clust = nrow(heatmap.data$D)
      } else {
        num_clust = input$num_clust
      } 
      
      tree1 = cutree(hclust(dist(as.matrix(heatmap.data$D), method = input$dist), method = input$clust, members = heatmap.data$labels), num_clust)
      
      print(head(tree1))
      
      H1_carpet = t(D$carpet)
      H1_carpet = merge(H1_carpet, as.matrix(tree1), by = "row.names", sort = FALSE)
    }
  })
  
  
  # download plot
  
  # download cluster
  
  output$pseudocount <- renderText({
    if (input$log_transform == "log10")
    paste0("<b>Attention, a pseudo count of 1 has been added to the data in order to log transform the data. </b>")
  })
    # print app version
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  })
    
})
  