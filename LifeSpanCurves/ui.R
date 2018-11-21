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
  titlePanel(title = h1("Upload file and select columns", align = "center")),
  
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
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Semicolon = ";",
                               Comma = ",",
                               Tab = "\t"),
                   selected = ","),
      
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(All = "all",
                               Head = "head"),
                   selected = "all"),
      
      # Select variables to display ----
      uiOutput("day"),
      uiOutput("deaths"),
      uiOutput("censors"),
      uiOutput("factors"),
      submitButton('run survival analysis')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("survPlot"),
      tableOutput("rendered_file")
      # tabsetPanel(
      #   id = "dataset",
      #   tabPanel("FILE", DT::dataTableOutput("rendered_file"))
      # )
    )
    
  )
))
