#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

.libPaths("/srv/shiny-server/heatmap/libs")

library(shiny)
library(xlsx)
library(tidyverse)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # App title ----
  titlePanel(title = h1("Run a Survival Analysis", align = "left")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      tabsetPanel(
        tabPanel("Input and model settings",
                 # Input: Select a file ----
                 hr(),
                 fileInput("uploaded_file", "Choose File",
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".tsv",
                                      ".csv",
                                      ".xlsx")
                 ),
                 # Input: Select separator ----
                 radioButtons("filetype", 'Please select the input file type', choices = c('auto' = 'auto', 
                                                                                           "excel" = 'xlsx',  
                                                                                           'tab-separated' = '\t', 
                                                                                           'comma-seperated' = ',', 
                                                                                           'semicolon-separated' = ';'), inline = TRUE),
                 a(href = "https://github.com/mpg-age-bioinformatics/shiny/blob/master/LifeSpanCurves/input_examples/lifespan_test.csv", "Example input"),

                 # Input: Checkbox if file has header ----
                 checkboxInput("header", "Header", TRUE),
                 uiOutput("labels"),
                 radioButtons("dendro", "Dendrogram", choices = c("both","row","column","none"), selected = "both", inline = TRUE),
                 textInput('na.color', 'select color for NA', value = 'black'),
                 selectInput('clust', 'Select Cluster algortihm', choices = c("complete", "ward.D", "ward.D2", "single",  "average", "mcquitty", "median", "centroid")), 
                 selectInput('dist', 'Select distance', choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
                 sliderInput('num_clust', 'Select number of clusters', min = 1, max = 20, value = 4, step = 1),
                 radioButtons("table", "Show cluster table", choices = c("yes" = TRUE,"no" = FALSE), selected = TRUE, inline = TRUE),
                 
                 submitButton('generate heatmap')
                 
        ),
        tabPanel("Plot settings",
                 textInput('color', 'color scheme', value = 'blue, white, red'),
                 textInput('color2', 'cluster color scheme', value = 'green, white, black, blue'),
                 radioButtons('log_transform', 'Transform data to log scale', choices = c("none", "log10", "log2"), selected = "none", inline = TRUE),
                 radioButtons('heat_scale', 'Heatmap scaling function', choices = c("none", "row", "col"), selected = "none", inline = TRUE),
                 sliderInput('cex_lab', 'Select label size', min = 0.1, max = 3, value = 1, step = 0.1),
                 sliderInput('num_col', 'Select gradient size', min = 2, max = 50, value = 10, step = 1),
                 sliderInput('mar_right', 'Select right margin', min = 1, max = 50, value = 5, step = 0.1),
                 sliderInput('mar_bottom', 'Select bottom margin', min = 1, max = 50, value = 10, step = 0.1),
                 sliderInput('plot_width', 'Select plot width', min = 200, max = 5000, value = 600, step = 10),
                 sliderInput('plot_height', 'Select plot height', min = 200, max = 5000, value = 400, step = 10),
                 radioButtons('color_key', 'Inlcude Color Key', choices = c('yes' = TRUE, 'no' = FALSE), selected = TRUE, inline = TRUE),

                 submitButton('generate heatmap')
                 
            )
          )
      
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          plotOutput("heatmap", height = 'auto', width = 'auto'),
          hr(),
          # verbatimTextOutput("survStats"),
          # downloadButton('downloadPlot', 'Download Plot'),
          # downloadButton('downloadReport', 'Generate Report'),
          # hr(),
          tableOutput("clusterMap"),
          # downloadButton('downloadTable', 'Download Table'),
          # br(),br(),
          htmlOutput("pseudocount"),
          p("This App uses the", code('survival'), " package. For more information read the respective documentation in ",
            a("cran", href = "https://cran.r-project.org/web/packages/survival/index.html"),
            "and wikipedia's entry for ", a("survival analysis.",href="https://en.wikipedia.org/wiki/Survival_analysis" )),
          p("Please keep the version tag on all downloaded files."),
          htmlOutput('appversion')
        )
        
      )
))
  