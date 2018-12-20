.libPaths("/srv/shiny-server/mds/libs")
library(shiny)
# Define UI for application that draws a histogram

clusteralgs<-c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
distmeths<-c("euclidean", "maximum", "manhattan", "canberra", "binary" , "minkowski")

shinyUI( fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Input and cluster settings",
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
          helpText(a(href = "https://github.com/mpg-age-bioinformatics/shiny/blob/master/heatmap/heatmap_example.csv", "Example input")),
          hr(),
          numericInput("cluster", "Number of clusters", 1),
          checkboxInput("names", "Names", FALSE),
          checkboxInput("ellipse", "Ellipse", FALSE),
          hr(),
          textInput("outfile", "Output file name", value="MDS")),
        tabPanel("Plot settings",
                 selectInput("method", "Select distance method", choices = distmeths,selected="euclidean"),
                 sliderInput('dimension', 'Select number of dimensions', min = 0, max = 10, value = 2, step = 1),
                 #textInput("dimension.names", "Dimensions names", "Dim.1,Dim.2"),
                 selectInput("x", "X-axis dimension", choices=NULL),
                 selectInput("y", "Y-axis dimension", choices=NULL),
                 sliderInput('ellipse.prob', 'Ellipse probability', min = 0, max = 1, value = 0.99, step = 0.001),
                sliderInput('symbols.size', 'Symbols size', min = 0, max = 10, value = 1, step = 0.01),
                numericInput("lowerx", "X-axis lower limit", value = NA),
                numericInput("upperx", "X-axis upper limit", value = NA),
                numericInput("lowery", "Y-axis lower limit", value = NA),
                numericInput("uppery", "Y-axis upper limit", value = NA),
                textInput("title", "MDS title", value="MDS")
        )
      )
    ),
    mainPanel(
      plotOutput("mds", height = '500px', width = 'auto'),
      downloadButton('downloadPlot', 'Download Plot'),
      br(), br(),
      p("This App uses R's ", code('dist'), ",", code('cmdscale'), "and", code('kmeans') ,". For more information read the respective documentation in the R-manual",
        a("(dist", href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html"),
        ",",
        a("cmdscale)", href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cmdscale.html"),
        "and",
        a("kmeans)", href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html"),
        
        "and wikipedia's entry for ", a("MDS",href="https://en.wikipedia.org/wiki/Multidimensional_scaling" ),
        "."),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
      )
  )
))
