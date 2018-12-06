.libPaths("/srv/shiny-server/histogram/libs")
library(shiny)
# Define UI for application that draws a histogram
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
      selectInput("column", "Select Column", choices = NULL),
      helpText(a(href = "https://raw.githubusercontent.com/mpg-age-bioinformatics/shiny/master/histogram/chol.txt", "Example input")),
      hr(),
      numericInput("lowerx", "X-axis lower limit", NULL),
      numericInput("upperx", "X-axis upper limit", NULL),
      numericInput("lowery", "Y-axis lower limit", NULL),
      numericInput("uppery", "Y-axis upper limit", NULL),
      textInput("title", "Histogram title", value="Histogram"),
      textInput("xlabel", "X-axis label"),
      numericInput("breaks", "Number of breaks", value=NULL),
      checkboxInput("probability", "Probability", TRUE),
      checkboxInput("density", "Density", TRUE),
      sliderInput('linewidth', 'Select line width', min = 0, max = 5, value = 1, step = 0.1),
      hr(),
      textInput("outfile", "Output file name", value="Histogram")#,
      #submitButton('generate plot')
    ),
    mainPanel(
      #textOutput("testtext"),
      plotOutput("histogram", height = "500px", width = "500px"),
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
