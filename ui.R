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
    fluidPage(
        theme=shinytheme("yeti"),
        useShinyjs(), 
        # theme = "united.css",
        titlePanel(""),
        
        shinyjs::hidden(textInput("hidden", "Will be hidden", "foo")),
        #textInput("hidden", "Will be hidden", "foo"),
        
        fluidRow(
            box(
                tabsetPanel(id='tabs', 
                            type="pills",
                            tabPanel("Cases", id='Cases', plotlyOutput("casesplot")),
                            tabPanel("Deaths", id='Deaths', plotlyOutput("deathsplot")))
            ),
            box(
                leafletOutput("map1", height = 442)
            )
        ),
        
        fluidRow(
            box(width=6,
                title=("Policy Details"), 
                #tags$hr(),
                status="primary",
                solidHeader = T,
                tagAppendAttributes(textOutput("click"), style="white-space:pre-wrap;")
            ),
            
            box(width = 6,
                title=("About this Page and Visualization"),
                "This timeline shows the major COVID-19 infection control measures, re-openings, and vaccination roll-out undertaken by the state of Connecticut after the first COVID-19 case appeared on March 6th, 2020 and the first executive order of ‘STATE EMERGENCY’ declared on March 10th, 2020. 
                Along with the timeline of policy decisions", tags$b("we have a graph of daily cases, and maps of daily cases and deaths "), "over the same time period. 
                The dotted lines indicate key policy decisions –" , tags$b("restrictions/closings (in red), openings (in green), interventions (in orange), vaccination (in blue) and other (in black)"),". 
                Clicking on a line will display", tags$em("Policy Details"), "below the chart. 
                Additionally, the control buttons on top of the timeline allow users to sift through key events by day, allowing them to be viewed in chronological order. 
                The timeline can be viewed against either new cases or new deaths. 
                To see current cumulative case and death counts and daily counts for days with no policy events, go to our",tags$a(href="https://s.uconn.edu/covid-map", "Dashboard."),
                #tags$hr(),
                status="primary",
                fill = FALSE,
                solidHeader = T
            )
        ),
    )
)

##
dashboardPage(
    dashboardHeader(disable=TRUE),
    dashboardSidebar(disable=TRUE),
    body
)
