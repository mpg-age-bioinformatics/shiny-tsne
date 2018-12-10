.libPaths("/srv/shiny-server/scatterplot/libs")
library(shiny)
# Define UI for application that draws a histogram

symbols<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
           32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,
           54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
           80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,
           106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127)



shinyUI( fluidPage(
  titlePanel("Scatterplot"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Input Data",
                 fileInput("uploaded_file", "Choose File",
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
          uiOutput("x"),
          uiOutput("y"),
          helpText(a(href = "https://raw.githubusercontent.com/mpg-age-bioinformatics/shiny/master/scatterplot/test_dataframe.csv", "Example input")),
          hr(),
          textInput("outfile", "Output file name", value="Scatterplot")#,
          #submitButton('generate plot')
          
        ),
        tabPanel("Plot settings", 
               
          # numericInput("lowerx", "X-axis lower limit", value = NA),
          # numericInput("upperx", "X-axis upper limit", value = NA),
          # numericInput("lowery", "Y-axis lower limit", value = NA),
          # numericInput("uppery", "Y-axis upper limit", value = NA),
          textInput("title", "Plot title", value="Scatter plot"),
          textInput("xlabel", "X-axis label", "x-axis"),
          textInput("ylabel", "Y-axis label", "y-axis"),
          sliderInput("fontsize.title", "Title font size", min = 8, max = 32, value = 11.5, step = 0.5),
          sliderInput("fontsize.axis", "Axes font size", min = 8, max = 32, value = 11.5, step = 0.5),
          sliderInput("fontsize.legend", "Legend font size", min = 8, max = 32, value = 11.5, step = 0.5),
          selectInput("pch","Symbols", choices = symbols, selected = 19),
          radioButtons('showGrid', "Show grid", choices = c("yes" = TRUE, "no" = FALSE), selected = TRUE, inline = TRUE),
          helpText(a(href = "http://sape.inf.usi.ch/quick-reference/ggplot2/shape", "Symbols map")),
          sliderInput('scex', 'Symbols size', min = 0.1, max = 10, value = 2, step = 0.1),
          radioButtons('colortype', "Please select variable type", choices = c('single','factor', 'continious'), inline = TRUE, selected = 'single'),
          uiOutput("factors"),
          textInput("color", 'Comma seperated list of colors', value = 'black'),
          radioButtons("showLegend", "Show legend", choices = c("yes" = TRUE, "no" = FALSE), selected = FALSE, inline = TRUE),
          hr()
        )
      )
    ),
    mainPanel(
             fluidRow(
               column(width = 6, height = 6,
                      h5("Use brush to zoom, displayed in right plot ->"),
                      plotOutput("scatterplot1", click = "plot_click" ,
                                 brush = brushOpts(
                                   id = "plot1_brush",
                                   resetOnNew = TRUE
                                 )
                      )
               ),
               column(width = 6, height = 6,
                      h5("Use brush and double click to further zoom in"),
                      plotOutput("scatterplot2", click = "plot_click", dblclick = "plot2_dblclick",
                                brush = brushOpts(
                                  id = "plot2_brush",
                                  resetOnNew = TRUE) )
               )
             ),
      #),
      hr(),
      downloadButton('downloadPlot1', 'Download plot'),
      downloadButton('downloadPlot2', 'Download zoomed plot'),
      downloadButton('downloadTable', "Download table"),
      br(), br(),
      h4("Clicked points"),
      verbatimTextOutput("info2", placeholder = TRUE),
      h4("Brushed points"),
      verbatimTextOutput("info", placeholder = TRUE),
      p("This App uses R's ", code('ggplot2'), " package For more information read the respective documentation on ",
        a("cran", href = "https://cran.r-project.org/web/packages/ggplot2/index.html"), "."),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
    )
  )
))
