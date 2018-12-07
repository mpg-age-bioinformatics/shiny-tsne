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
  titlePanel("Heatmap"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      tabsetPanel(
        tabPanel("Input and cluster settings",
                 # Input: Select a file ----
                 hr(),
                 a(href = "https://github.com/mpg-age-bioinformatics/shiny/blob/master/heatmap/heatmap_example.csv", "Example input"),
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

                 # Input: Checkbox if file has header ----
                 checkboxInput("header", "Header", TRUE),
                 uiOutput("labels"),
                 radioButtons("dendro", "Dendrogram to plot", choices = c("both","row","column","none"), selected = "both", inline = TRUE),
                 textInput('na.color', 'Select color for NA', value = 'black'),
                 selectInput('clust', 'Select cluster algortihm', choices = c("complete", "ward.D", "ward.D2", "single",  "average", "mcquitty", "median", "centroid")), 
                 selectInput('dist', 'Select distance', choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
                 sliderInput('num_clust', 'Select number of clusters', min = 1, max = 20, value = 4, step = 1),
                 radioButtons("table", "Show cluster table", choices = c("yes" = TRUE,"no" = FALSE), selected = TRUE, inline = TRUE),
                 textInput('outfile', 'Output prefix', value = 'heatmap'),
                 
                 submitButton('generate heatmap')
                 
        ),
        tabPanel("Plot settings",
                 textInput('color', 'Color scheme', value = 'blue, white, red'),
                 textInput('color2', 'Cluster color scheme', value = 'green, white, black, blue'),
                 radioButtons('log_transform', 'Transform data to log scale', choices = c("none", "log10", "log2"), selected = "none", inline = TRUE),
                 radioButtons('heat_scale', 'Heatmap scaling function', choices = c("none", "row", "col"), selected = "none", inline = TRUE),
                 sliderInput('cex_lab', 'Select label size', min = 0.1, max = 3, value = 2, step = 0.1),
                 sliderInput('num_col', 'Select gradient size', min = 2, max = 50, value = 10, step = 1),
                 sliderInput('mar_right', 'Select right margin', min = 1, max = 50, value = 10, step = 0.1),
                 sliderInput('mar_bottom', 'Select bottom margin', min = 1, max = 50, value = 20, step = 0.1),
                 sliderInput('plot_width', 'Select plot width', min = 200, max = 5000, value = 1000, step = 10),
                 sliderInput('plot_height', 'Select plot height', min = 200, max = 5000, value = 750, step = 10),
                 radioButtons('color_key', 'Inlcude color key', choices = c('yes' = TRUE, 'no' = FALSE), selected = TRUE, inline = TRUE),

                 submitButton('generate heatmap')
                 
            ),
        tabPanel("Filtering",
                 br(),
                 uiOutput('filter_cluster'),
                 textInput('filter_row', 'Select rows (comma seperated list of IDs)', value = 'all'),
                 radioButtons("dendro.sub", "Dendrogram", choices = c("both","row","column","none"), selected = "both", inline = TRUE),
                 sliderInput('num_clust.sub', 'Select number of sub clusters', min = 1, max = 20, value = 1, step = 1),
                 
                 submitButton('generate heatmap')
                 
            )
          )
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          plotOutput("heatmap", height = 'auto', width = 'auto'),
          hr(),
          downloadButton('downloadPlot', 'Download plot'),
          downloadButton('downloadTable', 'Download table'),
          downloadButton('downloadTable2', 'Download filtered table'),
          DT::dataTableOutput("clusterMap2"),
          DT::dataTableOutput("clusterMap"),
          br(),br(),
          htmlOutput("pseudocount"),
          p("This App uses the", code('heatmap.2'), "function of the ", code('gplots'), " package. For more information read the respective documentation on ",
            a("cran. ", href = "https://www.rdocumentation.org/packages/gplots/versions/3.0.1/topics/heatmap.2"), code("heatmap.2"),
            "uses ", a("hclust",href="https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/hclust" ), " for clustering and ", 
a("dist",href="https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/dist" ), " for calculating the distance matrix. " ),
          p("Please keep the version tag on all downloaded files."),
          htmlOutput('appversion')
        )
        
      )
))
  