.libPaths("/srv/shiny-server/cellplot/libs")
library(shiny)
# Define UI for application that draws a histogram
shinyUI( fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "DAVID's Functional Annotation file",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".tsv",
                           ".xlsx")
      ),
      radioButtons("filetype", "Please select DAVID's Functional Annotation file type", choices = c('auto' = 'auto', 
                                                                                                    "excel" = 'xlsx',  
                                                                                                    'tab-separated' = '\t', 
                                                                                                    'comma-seperated' = ',', 
                                                                                                    'semicolon-separated' = ';'), inline = TRUE),
      checkboxInput("header", "Header", TRUE),
      helpText(a(href = "https://raw.githubusercontent.com/mpg-age-bioinformatics/shiny/master/histogram/chol.txt", "Example input")),
      hr(),
      fileInput("file2", "Log2FC and P Adj. reference file",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".tsv",
                           ".xlsx")
      ),
      radioButtons("filetype2", "Please select Log2FC and P Adj. reference file type", choices = c('auto' = 'auto', 
                                                                                                   "excel" = 'xlsx',  
                                                                                                   'tab-separated' = '\t', 
                                                                                                   'comma-seperated' = ',', 
                                                                                                   'semicolon-separated' = ';'), inline = TRUE),
      checkboxInput("header2", "Header", TRUE),
      helpText(a(href = "https://raw.githubusercontent.com/mpg-age-bioinformatics/shiny/master/histogram/chol.txt", "Example input")),
      hr(),
      selectInput("categories","Select Categories", choices = NULL, multiple = TRUE, selected = NULL),
      selectInput("genessel","Select Genes Name/ID Column",  choices = NULL, selected = NULL),
      selectInput("logfcsel","Select Log2(FC) Column", choices = NULL, selected = NULL),
      selectInput("padjsel","Select P Adj. Column", choices = NULL, selected = NULL),
      sliderInput('nterms',  "Number of terms to plot", min = 1, max = 20, value = 10, step = 1),
      hr(),
      textInput("outfile", "Output file name", value="cellplots"),
      submitButton('submit')
    ),
    mainPanel(
      plotOutput("cellplot", height = "500px", width = "500px"),
      br(), br(),
      p("This App uses the", code('cellplot'), " package. For more information read the respective documentation in ",
        a("github", href = "http://htmlpreview.github.io/?https://github.com/dieterich-lab/CellPlot/blob/master/vignettes/CellPlotManual.html"),
        "."
      ),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
    )
  )
))

