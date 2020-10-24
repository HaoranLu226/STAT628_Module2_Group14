#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/ 
#
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2) 
library(png)
cleaned = read.csv("cleaned_bodyfat.csv")
shinyUI(fluidPage(
    
    titlePanel("Get Your BodyFat!"),
    
    hr(),
    
    fluidRow(
        column(3,
               numericInput("weight", "Weight:", value=154.25),
               numericInput("abdomen", label = "Abdomen:", value = 85.2),
               numericInput("wrist", label = "Wrist:", value = 17.1),
               submitButton("Calculate!", icon("refresh")),
               h3(span("Bodyfat (%):", verbatimTextOutput("res"), style = "font-weight: 300; float:left"), 
                  style = "font-family: 'Source Sans Pro'; text-align: left; padding: 20px; float:left")
               
        ),
        column(3,
               selectInput("weight_unit", "Weight Unit:", choices = c(pound="lbs", kilogram="kg")),
               selectInput("abdo_unit", "Abdomen Unit:", choices = c(centimeter = "cm", inches = 'inches')),
               selectInput("wrist_unit", "Wrist Unit:", choices = c(centimeter = "cm", inches = 'inches')),
               p("Recommended Reading:"),
               uiOutput("reduce"),
               p(),
               uiOutput("measure"),
               uiOutput("m_abdomen")
        ),
        column(3,
               imageOutput("myImage")
               
        ),
        column(3,
               
               p("Please contact us for any problem! "),
               p("We can be reached at: "),
               p("zhaobiubiu000@gmail.com"),
               p('xmiao27@wisc.edu'),
               p("hlu226@wisc.edu"),
               hr(),
               uiOutput("github"),
               uiOutput("picture")
               
        ),
        
        plotOutput("Plot")
)))



