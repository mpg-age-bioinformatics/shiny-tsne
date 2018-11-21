#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Read file ----
  df <- reactive({
    req(input$uploaded_file)
    read.csv(input$uploaded_file$datapath,
             header = input$header,
             sep = input$sep)  
    
  })
  
  # Dynamically generate UI input when data is uploaded ----
  output$day <- renderUI({
    selectInput(inputId = "day", 
                       label = "Select time", 
                       choices = names(df()))
  })
  
  output$deaths <- renderUI({
    selectInput(inputId = "death", 
                       label = "Select deaths", 
                       choices = names(df()))
  })
  
  output$censors <- renderUI({
    selectInput(inputId = "censor",
                       label = "Select censors",
                       choices = names(df()))
  })
  
  output$factors <- renderUI({
    checkboxGroupInput(inputId = "factors", 
                       label = "Select variables", 
                       choices = names(df()))
  })
  
  
  
  # Select columns and reformat table for survival analysis ----
  df_sel <- reactive({
    req(input$day, input$death, input$censor)
    df_sel <- df() %>% select(input$day, input$death, input$censor, input$factors)
    
    D = df_sel
    num_var = length(input$factors)
    D.survival <- as.data.frame(matrix(0, nrow = sum(D[,c(input$death, input$censor)]), ncol = 2 + num_var))
    names(D.survival) <- c('day', 'status', input$factors) 

    r = 1 # row counter for D.survival
    for(i in 1:nrow(D)){
      if(D[i, input$death] > 0){
        for(event in 1:D[i,input$death]){
          D.survival[r,c(1,2)] <- c(D[i,input$day], 1)
          if(num_var > 0){
            D.survival[r, c(3:ncol(D.survival))] <- as.character(D[i, input$factors])
            }
          r = r+1
        }
      }
      if(D[i, input$censor] > 0){
          for(event in 1:D[i,input$censor]){
            D.survival[r,c(1,2)] <- c(D[i,input$day], 0)
            if(num_var > 0){
              D.survival[r, c(3:ncol(D.survival))] <- as.character(D[i, input$factors])
            }
            r = r+1
          }
        }
    }
    D.survival
    
  })
  
  output$survPlot <- renderPlot({
    num_var = length(input$factors)
    if(num_var == 0){
      fit <- survfit(Surv(day, status) ~ 1, data = df_sel())
      plot(fit, col = 'black', lty = 1, lwd = 1.5, conf.int = TRUE, main = 'Survival', xlab = 'days')
    } else {
      rename_variables <- paste('group', 1:num_var, sep = '_')
      plot.data = df_sel()
      names(plot.data) = c('day', 'status', rename_variables)
      if(num_var == 1){
        fit <- survfit(Surv(day, status) ~ group_1, data = plot.data)
        plot(fit, col = 1, lty = c(1,2), lwd = 1.5)
        
      } else if(num_var == 2){
        fit <- survfit(Surv(day, status) ~ group_1 + group_2, data = plot.data)
        plot(fit, col = rep(1:num_var, times = num_var), lty = rep(1:num_var, each = num_var), lwd = 1.5)
        
      } else if (num_var == 3){
        fit <- survfit(Surv(day, status) ~ group_1 + group_2 + group_3, data = plot.data)
        plot(fit, col = rep(1:num_var, times = num_var), lty = rep(1:num_var, each = num_var), lwd = 1.5)
      } else if (num_var > 3){
        print(paste("You have selected", num_var, "variables\nPlease select only three or less"))
        return(NULL)
      }
    }
  })

  # Print data table ----  
  output$rendered_file <- renderTable({
    if(input$disp == "head") {
      head(df_sel())
    }
    else {
      df_sel()
    }
  })
  
})
