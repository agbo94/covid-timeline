#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(htmlwidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(shinyjs)
library(shinydashboard)
library(shinythemes)


body <- dashboardBody(
    shinyUI(
        
        fluidPage(theme=shinytheme("lumen"),
            useShinyjs(), 
            # theme = "united.css",
            titlePanel(""),
            
            shinyjs::hidden(textInput("hidden", "Will be hidden", "foo")),
            #textInput("hidden", "Will be hidden", "foo"),
            
            fluidRow(
                column(7,
                       fluidRow(
                           tabsetPanel(id='tabs',
                                       type="pills",
                                       tabPanel("Cases", id='Cases', plotlyOutput("casesplot")),
                                       tabPanel("Deaths", id='Deaths', plotlyOutput("deathsplot"))
                           )
                       ),
                ),
                
                column(5,
                       fluidRow(leafletOutput("map1"))
                ),
                
                fluidRow(  
                    box(title=(tags$h5("Policy Details")), 
                        tags$hr(),
                        status="primary",
                        solidHeader = TRUE,
                        tagAppendAttributes(textOutput("click"), style="white-space:pre-wrap;")
                    )
                    )
            ),
        )
    )
)

##
dashboardPage(
    dashboardHeader(disable=TRUE),
    dashboardSidebar(disable=TRUE),
    body
)
