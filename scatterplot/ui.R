.libPaths("/srv/shiny-server/scatterplot/libs")
library(shiny)
# Define UI for application that draws a histogram

symbols<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
           32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,
           54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
           80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,
           106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127)


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
      selectInput("columnx","Select X-axis", choices = c("--select--"),selected = "--select--"),
      selectInput("columny","Select Y-axis", choices = c("--select--"),selected = "--select--"),
      helpText(a(href = "https://raw.githubusercontent.com/mpg-age-bioinformatics/shiny/master/histogram/chol.txt", "Example input")),
      hr(),
      numericInput("lowerx", "X-axis lower limit", value = NA),
      numericInput("upperx", "X-axis upper limit", value = NA),
      numericInput("lowery", "Y-axis lower limit", value = NA),
      numericInput("uppery", "Y-axis upper limit", value = NA),
      textInput("title", "Plot title", value="Scatter plot"),
      textInput("xlabel", "X-axis label", "x-axis"),
      textInput("ylabel", "Y-axis label", "y-axis"),
      selectInput("pch","Symbols", choices = symbols, selected = 19),
      helpText(a(href = "http://sape.inf.usi.ch/quick-reference/ggplot2/shape", "Symbols map")),
      sliderInput('scex', 'Symbols size', min = 0.1, max = 10, value = 2, step = 0.1),
      hr(),
      textInput("outfile", "Output file name", value="Scatterplot")#,
      #submitButton('generate plot')
    ),
    mainPanel(
      #textOutput("testtext"),
      plotOutput("scatterplot", height = "500px", width = "500px", brush = "plot_brush", click = "plot_click"),
      verbatimTextOutput("info"),
      downloadButton('downloadPlot', 'Download Plot'),
      br(), br(),
      p("This App uses R's ", code('hist'), " function. For more information read the respective documentation in ",
        a("rdocumentation.org", href = "https://www.rdocumentation.org/packages/graphics/versions/3.5.1/topics/hist"),
        "and wikipedia's entry for ", a("histogram.",href="https://en.wikipedia.org/wiki/Histogram" )),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
    )
  )
))
