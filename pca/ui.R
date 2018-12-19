.libPaths("/srv/shiny-server/pca/libs")
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
          checkboxInput("scale", "Scale", FALSE),
          checkboxInput("arrows", "Arrows/Variables", FALSE),
          checkboxInput("varname.abbrev", "Abreviate variable name", FALSE),
          checkboxInput("names", "Names", FALSE),
          checkboxInput("ellipse", "Ellipse", FALSE),
          hr(),
          textInput("outfile", "Output file name", value="PCA")),
        tabPanel("Plot settings",
          numericInput("x", "X-axis component", 1),
          numericInput("y", "Y-axis component", 2),
          sliderInput('ellipse.prob', 'Ellipse probability', min = 0, max = 1, value = 0.68, step = 0.01),
          sliderInput('labelssize', 'Select labels size', min = 0, max = 5, value = 3, step = 0.1),
          sliderInput('alpha', 'Alpha', min = 0, max = 1, value = 1, step = 0.01),
          sliderInput('varname.size', 'Variable name size', min = 0, max = 5, value = 3, step = 0.01),
          sliderInput('varname.adjust', 'Variable name adjust', min = 0, max = 5, value = 1.5, step = 0.01),
          textInput("colors", "Clusters colors", value=NULL),
          textInput("groups", "Groups names", value="Groups"),
          numericInput("lowerx", "X-axis lower limit", value = NA),
          numericInput("upperx", "X-axis upper limit", value = NA),
          numericInput("lowery", "Y-axis lower limit", value = NA),
          numericInput("uppery", "Y-axis upper limit", value = NA),
          textInput("title", "PCA title", value="PCA")
        )
      )
    ),
    mainPanel(
      plotOutput("PCA", height = '500px', width = 'auto'),
      downloadButton('downloadPlot', 'Download Plot'),
      br(), br(),
      p("This App uses R's ", code('prcomp'), " and the ggbiplot package function. For more information read the respective documentation in rdocumentation.org",
        a("(prcomp", href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html"),
        "and",
        a("ggbiplot)", href = "https://www.rdocumentation.org/packages/ggbiplot/versions/0.55/topics/ggbiplot"),
        "and wikipedia's entry for ", a("pca",href="https://en.wikipedia.org/wiki/Principal_component_analysis" ),
        "."),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
      )
  )
))
