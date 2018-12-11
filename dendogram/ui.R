.libPaths("/srv/shiny-server/dendogram/libs")
library(shiny)
# Define UI for application that draws a histogram

clusteralgs<-c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
distmeths<-c("euclidean", "maximum", "manhattan", "canberra", "binary" , "minkowski")

shinyUI( fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".tsv",
                           ".xlsx")
      ),
      radioButtons("filetype", 'Please select the input file type', choices = c('auto' = 'auto', 
                                                                                "excel" = 'xlsx',  
                                                                                'tab-separated' = '\t', 
                                                                                'comma-seperated' = ',', 
                                                                                'semicolon-separated' = ';'), inline = TRUE),
      checkboxInput("header", "Header", TRUE),
      selectInput("column", "Select labels column", choices = NULL,selected=NULL),
      helpText(a(href = "https://raw.githubusercontent.com/mpg-age-bioinformatics/shiny/master/histogram/chol.txt", "Example input")),
      hr(),
      checkboxInput("circular", "Circular", FALSE),
      uiOutput("trackheight"),
      selectInput("distance", "Select distance metric", choices = distmeths ,selected="euclidean"),
      selectInput("algor", "Select clustering algorithm", choices = clusteralgs, selected="ward.D2"),
      numericInput("clusters", "Number of clusters", 1),
      textInput("colors", "Clusters colors", value="rainbow"),
      textInput("title", "Dendogram title", value="Dendogram"),
      sliderInput('textsize', 'Select text size', min = 0, max = 5, value = 1, step = 0.1),
      sliderInput('labelssize', 'Select labels size', min = 0, max = 5, value = 1, step = 0.1),
      hr(),
      textInput("outfile", "Output file name", value="Dendogram")#,
      #submitButton('generate plot')
    ),
    mainPanel(
      plotOutput("dendogram", height = '500px', width = 'auto'),
      downloadButton('downloadPlot', 'Download Plot'),
      br(), br(),
      p("This App uses R's ", code('dist'), " and ",  code('hclust')," function. For more information read the respective documentation in rdocumentation.org",
        a("(dist", href = "https://www.rdocumentation.org/packages/proxy/versions/0.4-22/topics/dist"),
        "and",
        a("hclust)", href = "https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/hclust"),
        "and wikipedia's entries for ", a("dendogram",href="https://en.wikipedia.org/wiki/Dendrogram" ),
        "and",
        a("cluster analysis.",href="https://en.wikipedia.org/wiki/Cluster_analysis" )),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
      )
  )
))
