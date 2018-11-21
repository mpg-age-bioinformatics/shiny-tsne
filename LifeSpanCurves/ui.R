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
      
      # Input: Select a file ----
      fileInput("uploaded_file", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Checkbox if file has header ----
      a(href = "https://github.com/mpg-age-bioinformatics/shiny/blob/master/LifeSpanCurves/lifespan_test.csv", "Example input"),
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Semicolon = ";",
                               Comma = ",",
                               Tab = "\t"),
                   selected = ",", inline = TRUE),
      
      
      # Horizontal line ----
      tags$hr(),
      tags$h3("Select input and model for survival analysis", align = 'left'),
      
      # Select variables to display ----
      uiOutput("day"),
      uiOutput("deaths"),
      uiOutput("censors"),
      uiOutput("factors"),
      hr(),
      # make seperate tab
      tags$h3("Change plot settings:", align = 'left'),
      textInput('main', 'Plot title', value = 'Survival Analysis'),
      textInput('xlab', 'x-axis label', value = 'Time [Days]'),
      textInput('ylab', 'y-axis label', value = 'Survival'),
      
      sliderInput('main.size', 'Select font size for the title', min = 0, max = 5, value = 2, step = 0.1),
      sliderInput('lab.size', 'Select font size for the axis labels', min = 0, max = 5, value = 1.3, step = 0.1),
      sliderInput('axis.size', 'Select font size for the axis', min = 0, max = 5, value = 1.2, step = 0.1),
      sliderInput('legend.size', 'Select font size for the legend', min = 0, max = 5, value = 1.3, step = 0.1),
      sliderInput('linewidth', 'Select line width', min = 0, max = 5, value = 1, step = 0.1),
      sliderInput('margin.size', 'Select margin size', min = 1, max = 10, value = 4, step = 0.1),

      textInput('colors', "Comma seperated list of colors for frist variable", value = 'black'),
      a(href = "http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf", "R-colors"),
      checkboxGroupInput("linetype", "Select line type for second variable:", choices = c("solid" = 1,"dashed" = 2, "dotted" = 3, 
                                                                "dot-dashed" = 4, "longdashed" = 5, "blank" = 0), inline = TRUE, selected = 1),
      radioButtons('logaxis', 'Use log-scale', choices = c('none' ='', 'x-axis' = 'x', 'y-axis' = 'y', 'both' = 'xy'), inline = TRUE),
      radioButtons('conf', 'Plot confidence interval', choices = c("no" = FALSE, "yes" = TRUE), inline = TRUE),
      radioButtons('marks', 'Mark censored events', choices = c("no" = FALSE, "yes" = TRUE), inline = TRUE),
      
      hr(),
      checkboxInput('table', "Show table", FALSE),
      hr(),
      textInput("outfile", "Output file name", value="Survival"),
      submitButton('run survival analysis')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("survPlot"),
      downloadButton('downloadPlot', 'Download Plot'),
      tableOutput("survTab"),
      br(),br(),
      p("This App uses the", code('survival'), " package. For more information read the respective documentation in ",
        a("cran", href = "https://cran.r-project.org/web/packages/survival/index.html"),
        "and wikipedia's entry for ", a("survival analysis.",href="https://en.wikipedia.org/wiki/Survival_analysis" )),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
      )
    
  )
))
