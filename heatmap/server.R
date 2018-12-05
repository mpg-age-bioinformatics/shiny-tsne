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
library(DT)

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
  
  
  output$filter_cluster <- renderUI({
    selectInput(inputId = "filter_cluster", 
                label = "Select cluster for subsetting", 
                choices = c("all", 1:input$num_clust), 
                multiple = TRUE, 
                selected = "all", selectize = FALSE)
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
  
  genes <- reactive({
    genes <- input$filter_row
    genes <- gsub(' ', '', genes)
    genes <- unlist(strsplit(genes, ','))
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
  
  
  plot.heatmap.filtered <- reactive({
    req(input$uploaded_file, input$labels)
    
    if(is.null(input$filter_cluster) | ("all" %in% input$filter_cluster & genes() == "all")){
      heatmap.data = heatmap.data()
      dendro = input$dendro
      num_clust = input$num_clust
    } else if (input$filter_cluster != "all" & genes() == "all") {
      carpet = carpet()
      carpet = subset(carpet, cluster %in% input$filter_cluster)
      heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
      dendro = input$dendro.sub
      num_clust = input$num_clust.sub
    } else if (input$filter_cluster != "all" & genes() != "all") {
      carpet = carpet()
      carpet = subset(carpet, cluster %in% input$filter_cluster | carpet[, input$labels] %in% genes())
      heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
      dendro = input$dendro.sub
      num_clust = input$num_clust.sub
    } else {
      carpet = carpet()
      carpet = subset(carpet, carpet[, input$labels] %in% genes())
      heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
      dendro = input$dendro.sub
      num_clust = input$num_clust.sub
    }
    
    if(dendro == 'both'){
      Rowv = TRUE
      Colv = TRUE
    } else if (dendro == "row"){
      Rowv = TRUE
      Colv = FALSE
    } else if (dendro == 'column'){
      Rowv = FALSE
      Colv = TRUE
    } else {
      Rowv = FALSE
      Colv = FALSE
    }
    # name for color key
    
    HM <- heatmap.2(as.matrix(heatmap.data$D),labRow = heatmap.data$labels, trace = 'none', col = plot.color(),
                    margins = c(input$mar_bottom, input$mar_right), Rowv = Rowv, Colv = Colv, na.color = input$na.color,
                    dendrogram = input$dendro, key = input$color_key, 
                    distfun = function(x) dist(x, method = input$dist), 
                    hclustfun = function(x) hclust(x, method = input$clust), scale = input$heat_scale,
                    cexRow = input$cex_lab, cexCol = input$cex_lab)
    
    colors2 <- input$color2
    colors2 <- gsub(' ', '', colors2)
    colors2 <- unlist(strsplit(colors2, ','))
    
    if(num_clust > nrow(heatmap.data$D)){
      num_clust = nrow(heatmap.data$D)
    } else {
      num_clust = num_clust
    } 
    
    tree1 = cutree(as.hclust(HM$rowDendrogram), num_clust)
    nofclust.height <-  length(unique(as.vector(tree1)));
    selcol2 <- colorRampPalette(colors2)
    clustcol.height = selcol2(nofclust.height)
    
    HM <- heatmap.2(as.matrix(heatmap.data$D),labRow = heatmap.data$labels, trace = 'none', col = plot.color(),
                    margins = c(input$mar_bottom, input$mar_right), Rowv = Rowv, Colv = Colv, na.color = input$na.color,
                    dendrogram = input$dendro, key = input$color_key, 
                    distfun = function(x) dist(x, method = input$dist), 
                    hclustfun = function(x) hclust(x, method = input$clust), scale = input$heat_scale,
                    cexRow = input$cex_lab, cexCol = input$cex_lab, RowSideColors = clustcol.height[tree1])
  })
  
  
  
  
  carpet <- reactive({
    req(input$uploaded_file)
    D <- plot.heatmap()
    heatmap.data = heatmap.data()
    
    if(input$num_clust > nrow(heatmap.data$D)){
      num_clust = nrow(heatmap.data$D)
    } else {
      num_clust = input$num_clust
    } 
    
    tree1 = cutree(hclust(dist(as.matrix(heatmap.data$D), method = input$dist), method = input$clust, members = heatmap.data$labels), num_clust)
    tree1 = data.frame(ID = heatmap.data$labels, cluster = tree1)
    
    H1_carpet = t(D$carpet)
    row.names(H1_carpet) <- D$rowInd
    H1_carpet = merge(H1_carpet, tree1, by = "row.names", sort = FALSE)
    H1_carpet <- H1_carpet[,c("ID", "cluster", row.names(D$carpet))]
    H1_carpet <- H1_carpet[rev(row.names(H1_carpet)),c("ID", "cluster", row.names(D$carpet))]
    names(H1_carpet)[1] <- input$labels
    H1_carpet$heatmap_order <- seq(1:nrow(H1_carpet))
    H1_carpet
    
  })
  
  
  carpet.filtered <- reactive({
    req(input$uploaded_file)
    D <- plot.heatmap.filtered()
    
    if(is.null(input$filter_cluster) | ("all" %in% input$filter_cluster & genes() == "all")){
      heatmap.data = heatmap.data()
      dendro = input$dendro
      num_clust = input$num_clust
    } else if (input$filter_cluster != "all" & genes() == "all") {
      carpet = carpet()
      carpet = subset(carpet, cluster %in% input$filter_cluster)
      row.names(carpet) <- 1:nrow(carpet)
      heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
      dendro = input$dendro.sub
      num_clust = input$num_clust.sub
    } else if (input$filter_cluster != "all" & genes() != "all") {
      carpet = carpet()
      carpet = subset(carpet, cluster %in% input$filter_cluster | carpet[, input$labels] %in% genes())
      row.names(carpet) <- 1:nrow(carpet)
      heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
      dendro = input$dendro.sub
      num_clust = input$num_clust.sub
    } else {
      carpet = carpet()
      carpet = subset(carpet, carpet[, input$labels] %in% genes())
      row.names(carpet) <- 1:nrow(carpet)
      heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
      dendro = input$dendro.sub
      num_clust = input$num_clust.sub
    }
    
    
    if(input$num_clust.sub > nrow(heatmap.data$D)){
      num_clust = nrow(heatmap.data$D)
    } else {
      num_clust = input$num_clust.sub
    } 
    
    tree1 = cutree(hclust(dist(as.matrix(heatmap.data$D), method = input$dist), method = input$clust, members = heatmap.data$labels), num_clust)
    tree1 = data.frame(ID = heatmap.data$labels, cluster = tree1)
    
    H1_carpet = t(D$carpet)
    row.names(H1_carpet) <- D$rowInd
    H1_carpet = merge(H1_carpet, tree1, by = "row.names", sort = FALSE)
    H1_carpet <- H1_carpet[,c("ID", "cluster", row.names(D$carpet))]
    H1_carpet <- H1_carpet[rev(row.names(H1_carpet)),c("ID", "cluster", row.names(D$carpet))]
    names(H1_carpet)[1] <- input$labels
    H1_carpet$heatmap_order <- seq(1:nrow(H1_carpet))
    H1_carpet
    
  })
  
  
  
  observe({
    output$heatmap <- renderPlot({
      req(input$uploaded_file, input$labels)

      if(is.null(input$filter_cluster) | ("all" %in% input$filter_cluster & genes() == "all")){
        heatmap.data = heatmap.data()
        dendro = input$dendro
        num_clust = input$num_clust
      } else if (input$filter_cluster != "all" & genes() == "all") {
        carpet = carpet()
        carpet = subset(carpet, cluster %in% input$filter_cluster)
        heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
        dendro = input$dendro.sub
        num_clust = input$num_clust.sub
      } else if (input$filter_cluster != "all" & genes() != "all") {
        carpet = carpet()
        carpet = subset(carpet, cluster %in% input$filter_cluster | carpet[, input$labels] %in% genes())
        heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
        dendro = input$dendro.sub
        num_clust = input$num_clust.sub
      } else {
        carpet = carpet()
        carpet = subset(carpet, carpet[, input$labels] %in% genes())
        heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
        dendro = input$dendro.sub
        num_clust = input$num_clust.sub
    }
    
      if(dendro == 'both'){
        Rowv = TRUE
        Colv = TRUE
      } else if (dendro == "row"){
        Rowv = TRUE
        Colv = FALSE
      } else if (dendro == 'column'){
        Rowv = FALSE
        Colv = TRUE
      } else {
        Rowv = FALSE
        Colv = FALSE
      }
      # name for color key
      
      HM <- heatmap.2(as.matrix(heatmap.data$D),labRow = heatmap.data$labels, trace = 'none', col = plot.color(),
                      margins = c(input$mar_bottom, input$mar_right), Rowv = Rowv, Colv = Colv, na.color = input$na.color,
                      dendrogram = input$dendro, key = input$color_key, 
                      distfun = function(x) dist(x, method = input$dist), 
                      hclustfun = function(x) hclust(x, method = input$clust), scale = input$heat_scale,
                      cexRow = input$cex_lab, cexCol = input$cex_lab)
      
      colors2 <- input$color2
      colors2 <- gsub(' ', '', colors2)
      colors2 <- unlist(strsplit(colors2, ','))
      
      if(num_clust > nrow(heatmap.data$D)){
        num_clust = nrow(heatmap.data$D)
      } else {
        num_clust = num_clust
      } 
      
      tree1 = cutree(as.hclust(HM$rowDendrogram), num_clust)
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
  
  
  
  output$clusterMap <- DT::renderDataTable({
    if(input$table)
      datatable(
        carpet(),
        selection = list(mode = "multiple"),
        caption = "Heatmap Carpet",
        options = list(pageLength = nrow(carpet()))
      )
  })
  
  output$clusterMap2 <- DT::renderDataTable({
    if(input$table)
      datatable(
        carpet.filtered(),
        selection = list(mode = "multiple"),
        caption = "Heatmap Carpet",
        options = list(pageLength = nrow(carpet()))
      )
  })
  
  
  # download plot
  output$downloadPlot <- downloadHandler(
    # specify file name
    filename = function() {
      paste(input$outfile,".plot.",gitversion(),".pdf", sep = "")
    },
    content = function(filename){
      # open device
      pdf(filename, height = (input$plot_height/10), width = (input$plot_width/10))
      
      
      if(is.null(input$filter_cluster) | ("all" %in% input$filter_cluster & genes() == "all")){
        heatmap.data = heatmap.data()
        dendro = input$dendro
        num_clust = input$num_clust
      } else if (input$filter_cluster != "all" & genes() == "all") {
        carpet = carpet()
        carpet = subset(carpet, cluster %in% input$filter_cluster)
        heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
        dendro = input$dendro.sub
        num_clust = input$num_clust.sub
      } else if (input$filter_cluster != "all" & genes() != "all") {
        carpet = carpet()
        carpet = subset(carpet, cluster %in% input$filter_cluster | carpet[, input$labels] %in% genes())
        heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
        dendro = input$dendro.sub
        num_clust = input$num_clust.sub
      } else {
        carpet = carpet()
        carpet = subset(carpet, carpet[, input$labels] %in% genes())
        heatmap.data = list(D = carpet[, 3:(ncol(carpet)-1)], labels = carpet[,1] )
        dendro = input$dendro.sub
        num_clust = input$num_clust.sub
      }

            
      if(dendro == 'both'){
        Rowv = TRUE
        Colv = TRUE
      } else if (dendro == "row"){
        Rowv = TRUE
        Colv = FALSE
      } else if (dendro == 'column'){
        Rowv = FALSE
        Colv = TRUE
      } else {
        Rowv = FALSE
        Colv = FALSE
      }
      # name for color key
      
      heatmap.data = heatmap.data()
      HM <- plot.heatmap()
      
      colors2 <- input$color2
      colors2 <- gsub(' ', '', colors2)
      colors2 <- unlist(strsplit(colors2, ','))
      
      if(num_clust > nrow(heatmap.data$D)){
        num_clust = nrow(heatmap.data$D)
      } else {
        num_clust = num_clust
      } 
      
      tree1 = cutree(as.hclust(HM$rowDendrogram), num_clust)
      nofclust.height <-  length(unique(as.vector(tree1)));
      selcol2 <- colorRampPalette(colors2)
      clustcol.height = selcol2(nofclust.height)
      
      HM <- heatmap.2(as.matrix(heatmap.data$D),labRow = heatmap.data$labels, trace = 'none', col = plot.color(),
                      margins = c(input$mar_bottom * 5, input$mar_right * 5), Rowv = Rowv, Colv = Colv, na.color = input$na.color,
                      dendrogram = dendro, key = input$color_key, 
                      distfun = function(x) dist(x, method = input$dist), 
                      hclustfun = function(x) hclust(x, method = input$clust), scale = input$heat_scale,
                      cexRow = input$cex_lab * 5, cexCol = input$cex_lab * 5, RowSideColors = clustcol.height[tree1], keysize = 0.5, key.par = list( cex = input$cex_lab * 2 ))
      
       # close device
      dev.off()
    })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$outfile,".carpet.",gitversion(),".tsv", sep = "")
    },
    content = function(filename) {
      inFile <- input$uploaded_file
      
      if (is.null(inFile))
        return(NULL)
      if (input$table){
        write.table(carpet(), filename, row.names = FALSE, quote = FALSE, sep = '\t')
      }
    }
  )

  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$outfile,".filtered_carpet.",gitversion(),".tsv", sep = "")
    },
    content = function(filename) {
      inFile <- input$uploaded_file
      
      if (is.null(inFile))
        return(NULL)
      if (input$table){
        write.table(carpet.filtered(), filename, row.names = FALSE, quote = FALSE, sep = '\t')
      }
    }
  )
  
  
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
  