#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#library(DT)
library(tidyverse)
library(survival)
library(xlsx)

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
                 checkboxInput('longtable', "Table in long format", FALSE),
                 
                 # Input: Checkbox if file has header ----
                 a(href = "https://github.com/mpg-age-bioinformatics/shiny/blob/master/LifeSpanCurves/lifespan_test.csv", "Example input"),
                 checkboxInput("header", "Header", TRUE),
                 
                 
                 # Horizontal line ----
                 tags$hr(),
                 # Select variables to display ----
                 uiOutput("day"),
                 uiOutput("deaths"),
                 uiOutput("censors"),
                 uiOutput("factors"),
                 hr(),
                 # make seperate tab
                 
                 textInput('colors', "Comma seperated list of colors for frist variable", value = 'black'),
                 a(href = "http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf", "R-colors"),
                 hr(),
                 checkboxGroupInput("linetype", "Select line type for second variable:", choices = c("solid" = 1,"dashed" = 2, "dotted" = 3, 
                                                                                                     "dot-dashed" = 4, "longdashed" = 5, "blank" = 0), inline = TRUE, selected = 1),
                 radioButtons('interaction_term', 'Add interaction term', choices = c("no" = FALSE, "yes" = TRUE), inline = TRUE),
                 
                 hr(),
                 checkboxInput('table', "Show table", FALSE),
                 hr(),
                 textInput("outfile", "Output file name", value="Survival"),
                 submitButton('run survival analysis')
                 
        ),
        tabPanel("Plot settings", 
                 hr(),
                 textInput('main', 'Plot title', value = 'Survival Analysis'),
                 textInput('xlab', 'x-axis label', value = 'Time [Days]'),
                 textInput('ylab', 'y-axis label', value = 'Survival'),
                 
                 sliderInput('main.size', 'Select font size for the title', min = 0, max = 5, value = 2, step = 0.1),
                 sliderInput('lab.size', 'Select font size for the axis labels', min = 0, max = 5, value = 1.3, step = 0.1),
                 sliderInput('axis.size', 'Select font size for the axis', min = 0, max = 5, value = 1.2, step = 0.1),
                 sliderInput('legend.size', 'Select font size for the legend', min = 0, max = 5, value = 1.3, step = 0.1),
                 sliderInput('linewidth', 'Select line width', min = 0, max = 5, value = 1, step = 0.1),
                 sliderInput('boxwidth', 'Select box line width', min = 0, max = 5, value = 1, step = 0.1),
                 sliderInput('margin.size', 'Select margin size', min = 1, max = 10, value = 4.5, step = 0.1),
                 radioButtons('logaxis', 'Use log-scale', choices = c('none' ='', 'x-axis' = 'x', 'y-axis' = 'y', 'both' = 'xy'), inline = TRUE),
                 radioButtons('conf', 'Plot confidence interval', choices = c("no" = FALSE, "yes" = TRUE), inline = TRUE),
                 radioButtons('marks', 'Mark censored events', choices = c("no" = FALSE, "yes" = TRUE), inline = TRUE),
                 sliderInput('plot.height', 'Select plot height (inches)', min = 2, max = 20, value = 7, step = 0.1),
                 sliderInput('plot.width', 'Select plot width (inches)', min = 2, max = 20, value = 10, step = 0.1),
                 submitButton('run survival analysis')
        )
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("survPlot"),
      hr(),
      verbatimTextOutput("survStats"),
      downloadButton('downloadPlot', 'Download Plot'),
      downloadButton('downloadReport', 'Generate Report'),
      hr(),
      tableOutput("survTab"),
      downloadButton('downloadTable', 'Download Table'),
      br(),br(),
      p("This App uses the", code('survival'), " package. For more information read the respective documentation in ",
        a("cran", href = "https://cran.r-project.org/web/packages/survival/index.html"),
        "and wikipedia's entry for ", a("survival analysis.",href="https://en.wikipedia.org/wiki/Survival_analysis" )),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
      )
    
  )
))
