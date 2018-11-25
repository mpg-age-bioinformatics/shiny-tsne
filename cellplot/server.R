.libPaths("/srv/shiny-server/cellplot/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}
library(shiny)
library(CellPlot)
futile.logger::flog.threshold(futile.logger::ERROR, name = "cellplotLogger")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #ref.data <- reactive({
  #  inFileExp <- input$file2
  #  filetype2 <- input$filetype2
  #  #req(inFileExp)
  #  
  #  filetype_map <- c("xlsx" = 'xlsx',  'tsv' = '\t', 'csv' = ',', 'txt'=" ")
  #  if(filetype2 == 'auto'){
  #    file_extension =  unlist(strsplit(inFileExp$datapath, '[.]'))[length(unlist(strsplit(inFileExp$datapath, '[.]')))]
  #    if(file_extension %in% names(filetype_map)){
  #      filetype2 <- filetype_map[file_extension]
  #      names(filetype2) <- NULL
  #      
  #    } else {
  #      print(paste("wrong file format", file_extension))
  #      return(NULL)
  #    }
  #  }
  #  
  #  if(filetype2 == 'xlsx'){
  #    DD <- read.xlsx(inFileExp$datapath, sheetIndex = 1, header = input$header2)
  #  } else {
  #    DD <- read.csv(inFileExp$datapath, header = input$header2, sep = filetype2)
  #  }
  #  
  #  DD[DD == ''] <- NA
  #
  #  return(DD)
  #})
  
  
  # reformat input data
  plot.data <- reactive({
    inFile <- input$file1
    filetype <- input$filetype
    req(inFile)
    req(input$file2)
    
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
    
    ##### relabel headers from DAVID into cellplot standards
    # 
    # 
    D["GenesSignificant"]<-D$Genes
    
    genes<-D$GenesSignificant
    class(genes)
    genes<-lapply( genes, function(x) strsplit(toString(x), ", ")[[1]])
    D$GenesSignificant<-genes
    Categories<-unique(D$Category)

    
    #
    #
    
    #req(input$file2)
    
    inFileExp <- input$file2
    filetype2 <- input$filetype2
    #req(inFileExp)
    
    filetype_map <- c("xlsx" = 'xlsx',  'tsv' = '\t', 'csv' = ',', 'txt'=" ")
    if(filetype2 == 'auto'){
      file_extension =  unlist(strsplit(inFileExp$datapath, '[.]'))[length(unlist(strsplit(inFileExp$datapath, '[.]')))]
      if(file_extension %in% names(filetype_map)){
        filetype2 <- filetype_map[file_extension]
        names(filetype2) <- NULL
        
      } else {
        print(paste("wrong file format", file_extension))
        return(NULL)
      }
    }
    
    if(filetype2 == 'xlsx'){
      DD <- read.xlsx(inFileExp$datapath, sheetIndex = 1, header = input$header2)
    } else {
      DD <- read.csv(inFileExp$datapath, header = input$header2, sep = filetype2)
    }
    
    DD[DD == ''] <- NA
    
    
    
    #DD<-ref.data()
    varsgenes <- names(DD)
    varslogfc <- names(DD)
    varspadj <- names(DD)
    
    cat(file=stderr(), "here1", "\n")
    
    #observe({
    #  if(!is.null(input$file2))
    #    updateSelectInput(session,  "genessel","Select Genes Name/ID Column", choices = varsgenes)})
    
    updateSelectInput(session, "categories","Select Categories", choices = Categories)
    updateSelectInput(session, "genessel","Select Genes Name/ID Column", choices = varsgenes )
    updateSelectInput(session, "logfcsel","Select Log2(FC) Column", choices = varslogfc)
    updateSelectInput(session, "padjsel","Select P Adj. Column", choices = varspadj)
    
    req(input$categories)
    req(input$genessel)
    req(input$logfcsel)
    req(input$padjsel)
    
    cat(file=stderr(), input$categories, "\n")
    cat(file=stderr(), "genes", "\n")
    cat(file=stderr(), input$genessel, "\n")
    cat(file=stderr(), "log2fc", "\n")
    cat(file=stderr(), input$logfcsel, "\n")
    cat(file=stderr(), "padj", "\n")
    cat(file=stderr(), input$padjsel, "\n")
    cat(file=stderr(), "here3", "\n")
    
    refcol<-input$genessel
    DD$GenesSignificantb<-DD$GenesSignificant
    DD['GenesSignificant']<-DD$refcol
    dput(DD,file="/srv/shiny-server/cellplot/test.ref.R")
    
    #cat(file=stderr(), unlist(genes), "\n")
    
    
    #fcs<-
    #cat(file=stderr(), unlist(fcs), "\n")
    cat(file=stderr(), "here4", "\n")
    #cat(file=stderr(), unlist(head(D)), "\n")
    #cat(file=stderr(), unlist(head(DD)), "\n")
    
    
    D$log2FoldChange<-lapply( genes, function(x) DD[DD$GenesSignificant %in% x, 'log2FoldChange' ])
    D$padj<- lapply( genes, function(x) DD[DD$GenesSignificant %in% x, 'padj' ])
    
    D$LogEnrich<-D$Fold.Enrichment
    
    D<-D[D["Category"] == input$categories, ]
    
    D <- D[order(D$PValue),]
    D <- D[1:input$nterms,]
    
    return(D)
  })
  
  plot.cellplot<-reactive({
  
    x<-plot.data()
    dput(x,file="/srv/shiny-server/cellplot/test.object.R")
    cell.plot(x = setNames(x$LogEnrich, x$Term), 
              cells = x$log2FoldChange, 
              main ="GO enrichment", 
              x.mar = c(.4, 0), 
              key.n = 7, 
              y.mar = c(.1, 0), 
              cex = 1.6, 
              cell.outer = 3, 
              bar.scale = .7, 
              space = .2)
  })
  
  
  plot.symplot<-reactive({
    
    x<-plot.data()
    sym.plot(x = setNames(x$LogEnrich, x$Term), 
             cells = x$log2FoldChange, 
             x.annotated = x$Annotated, 
             main = "GO enrichment",
             x.mar = c(.47, 0), 
             key.n = 7, 
             cex = 1.6, 
             axis.cex = .8, 
             group.cex = .7) 
  })
  
  plot.arclot<-reactive({
    
    x<-plot.data()
    x$up <- lapply(Map(setNames, x$log2FoldChange, x$GenesSignificant), function (i) { i[i>0] })
    x$dwn <- lapply(Map(setNames, x$log2FoldChange, x$GenesSignificant), function (i) { i[i<0] })
    arc.plot(x = setNames(x$LogEnrich, x$Term), 
             up.list = x$up, 
             down.list = x$dwn, 
             x.mar = c(.9, .5))
  })
  
  plot.histogram<-reactive({
    x<-plot.data()
    y <- lapply(x, function (x) {
      x$Upregulated <- sapply(x$log2FoldChange, function (z) sum(z>0))
      x$Downregulated <- sapply(x$log2FoldChange, function (z) sum(z<0))
      x
    })
    yterms <- unique(unlist(lapply(y, function(x){
      x <- subset(x, pvalCutOff <= 0.05)
      x <- x[order(x$LogEnrich),]
      head(x, 9)$GO.ID
    })))
    
    par(mar = c(0,.5,2.5,8))
    go.histogram(y, go.alpha.term = "pvalCutOff", gene.alpha.term = "padj", 
                 min.genes = 5, max.genes = 1e10, go.selection = yterms, show.ttest = T,
                 main = "GO enrichment", 
                 axis.cex = 1, lab.cex = 1.5, main.cex = 1.5)
  })

  output$cellplot <- renderPlot({
    plot.cellplot()
  })
  
  output$symplot <- renderPlot({
    plot.symplot()
  }) 
  
  output$arcplot <- renderPlot({
    plot.arclot()
  })
  
  output$histogram <- renderPlot({
    plot.histogram()
  })
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  }
  )
})