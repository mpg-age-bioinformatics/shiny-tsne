#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
.libPaths("/srv/shiny-server/VennDiagram/libs")
library(shiny)

# Define UI for application that draws a histogram
shinyUI( fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose File",
                accept = c("text/csv",
                          "text/comma-separated-values,text/plain",
                          ".tsv",
                          ".csv",
                          ".xlsx")
      ),
      radioButtons("filetype", 'Please select the input file type', choices = c('auto' = 'auto', 
                                                                                "excel" = 'xlsx',  
                                                                                'tab-separated' = '\t', 
                                                                                'comma-seperated' = ',', 
                                                                                'semicolon-separated' = ';'), inline = TRUE),
      checkboxInput("header", "Header", TRUE),
      a(href = "https://raw.githubusercontent.com/mpg-age-bioinformatics/shiny/master/VennDiagram/venndiagram_example.xlsx", "Example input"),
      hr(),
      textInput('venncolors', "Comma seperated list of colors", value = 'gray'),
      a(href = "http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf", "R-colors"),
      hr(),
      sliderInput('num.fontsize', 'Select number font size', min = 0, max = 5, value = 2, step = 0.1),
      sliderInput('cat.fontsize', 'Select category font size', min = 0, max = 5, value = 2, step = 0.1),
      sliderInput('marginsize', 'Select margin size', min = 0, max = 0.5, value = 0.1, step = 0.01),
      sliderInput('linewidth', 'Select line width', min = 0, max = 5, value = 1, step = 0.1),
      radioButtons("linetype", "Select line type:", choices = c("solid" = 1,"dashed" = 2, "dotted" = 3, 
                                                                "dot-dashed" = 4, "longdashed" = 5, "blank" = 0), inline = TRUE),
      selectInput("cat.font", "Select Font:", choices = c('mono','Courier', 'sans','Helvetica', 
                                                          'serif','Times','AvantGarde','Bookman', 'Helvetica-Narrow',
                                                          'NewCenturySchoolbook', 'Palatino', 'URWGothic', 'URWBookman',
                                                          'NimbusMon', 'URWHelvetica','NimbusSan', 'NimbusSanCond', 
                                                          'CenturySch', 'URWPalladio', 'URWTimes','NimbusRom')),
      radioButtons("cat.face", "Select Face:", choices = c("plain","bold","italic","bold.italic"), inline = TRUE),
      hr(),
      checkboxInput('table', "Show table", FALSE),
      hr(),
      textInput("outfile", "Output file name", value="VennDiagram"),
      submitButton('generate plot')
    ),
    mainPanel(
      #textOutput("testtext"),
      plotOutput("venn", height = "500px", width = "500px"),
      downloadButton('downloadPlot', 'Download Plot'),
      tableOutput("overlap"),
      downloadButton('downloadTable', 'Download Table'),
      br(),br(),
      p("This App uses the", code('VennDiagram'), " package. For more information read the respective documentation in ",
        a("cran", href = "https://cran.r-project.org/web/packages/VennDiagram/index.html"),
        "and wikipedia's entry for ", a("venn diagram.",href="https://en.wikipedia.org/wiki/Venn_diagram" )),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
    )
  )
))
