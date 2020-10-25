# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/ 
#
# Created by Junxia Zhao.
# Revised by Xinran Miao.
# They worked together and either can be responsible.
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2) 
library(png)
# Please set the working directory to current directory.
cleaned = read.csv("../data/cleaned_bodyfat.csv")
ui = shinyUI(fluidPage(
    
    titlePanel("Get Males BodyFat!"),
    
    hr(),
    
    fluidRow(
      # A column to input values.
        column(3,
               numericInput("weight", "Weight:", value=154.25),
               numericInput("abdomen", label = "Abdomen:", value = 85.2),
               numericInput("wrist", label = "Wrist:", value = 17.1),
               submitButton("Calculate!", icon("refresh")),
               h3(span("Bodyfat (%):", verbatimTextOutput("res"), style = "font-weight: 300; float:left"), 
                  style = "font-family: 'Source Sans Pro'; text-align: left; padding: 20px; float:left")
               
        ),
        # A column to input units.
        column(3,
               selectInput("weight_unit", "Weight Unit:", choices = c(lb = "pounds", kg = "kilograms")),
               selectInput("abdo_unit", "Abdomen Unit:", choices = c(cm = "centimeter", inches = "inches")),
               selectInput("wrist_unit", "Wrist Unit:", choices = c(cm = "centimeter", inches = "inches")),
               p("Recommended Reading:"),
               uiOutput("reduce"),
               p(),
               uiOutput("measure"),
               uiOutput("m_abdomen")
        ),
        # A column for images
        column(3,
               imageOutput("myImage")
               
        ),
        # A column for other information.
        column(3,
               # Print out texts.
               p("Please contact us for any problem! "),
               p("We can be reached at: "),
               p("jzhao347@wisc.edu"),
               p('xmiao27@wisc.edu'),
               p("hlu226@wisc.edu"),
               hr(),
               # url links
               uiOutput("github"),
               uiOutput("picture")
               
        ),
        # The histogram
        plotOutput("Plot")
)))



