#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(fontawesome)
library(shinyWidgets)
library(shinythemes)
library(colourpicker)
library(shinyBS)
library(shinyalert)

markerColors = c(
    "red",
    "darkred",
    "lightred",
    "orange",
    "beige",
    "green",
    "darkgreen",
    "lightgreen",
    "blue",
    "darkblue",
    "lightblue",
    "purple",
    "darkpurple",
    "pink",
    "cadetblue",
    "white",
    "gray",
    "lightgray",
    "black"
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("readable"),
    # Application title
    titlePanel("Travel Map Maker Tool"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
           "Welcome to the traval map maker tool! This tool lets you create and customize interactive maps of whre you have traveled and lets you share the result with your friends and family.",
           dropdownButton(
               fileInput(
                   "file1",
                   "Choose your CSV File",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
               ),
               
           radioButtons(
               "seperator",
               label = 'Separator',
               choices = c(
                   Comma = ',',
                   Semicolon = ';',
                   Tab = '\t',
                   Space = ''
               ),
               selected = ','
           ), 
                fileInput(
                    "file2",
                    "Choose your folder with images",
                    multiple = T,
                    accept = c('image/*')
                ),
                circle = FALSE,
                label = "Upload Data",
                icon = icon("upload"),
                tooltip = tooltipOptions(title = "Here you can upload your data and images you would like to have in your map.")
                
            ),
            br(),
            #verbatimTextOutput("coor_range", placeholder = TRUE),
            
            br(),
            actionButton(
                inputId = "success",
                label = "Launch a success sweet alert",
                icon = icon("check")
            ),
            br(),
            actionButton(
                inputId = "error",
                label = "Launch an error sweet alert",
                icon = icon("remove")
            ),
            br(),
            br(),
            # Icons
            dropdownButton(
                selectInput(
                    "markerColor",
                    "Select color of the marker around the icon",
                    choices = markerColors
                ),
                checkboxInput("square_markers", "Square markers", F),
                selectInput(
                    'icon',
                    'Select icon',
                    choices = fa_metadata()$icon_names,
                    selected = "map-pin"
                ),
                colourInput("icon_color", "Select icon_colour", "white"),
                checkboxInput("icon_spin", "Spinning icons", F),
                label = "Marker Options",
                icon = icon("thumbtack"),
                circle = FALSE
            ),
            br(),
            dropdownButton(
                textAreaInput("map_title", "Map title"),
                numericInput(
                    "title_size",
                    "Title font size:",
                    value = 19,
                    min = 12,
                    max = 35
                ),
                colourInput("title_color", "Select title_colour", "black"),
                label = "Title Options",
                icon = icon("font"),
                circle = FALSE
            ),
            br(),
            checkboxInput(
                "wrap_map",
                "Extend the edges of the map. (This may duplicate elements.)",
                F
            ),
            #checkboxInput("cluster", "Cluster points when zoomed out", F),
            br(),
            # download buttons
            dropdownButton(
                downloadButton("downloadPlotPNG", "Download PNG"),
                downloadButton("downloadPlotHTML", "Download HTML"),
                circle = FALSE,
                label = "Download Options",
                icon = icon("download")
            ),
           textInput("chd","Day seperator","d"),
           textInput("chm","Minute seperator","'"),
           textInput("chs","Second seperator","\""),
           checkboxInput("dms", "DMS", F)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map", height = 600),
            dataTableOutput("contents"),
            verbatimTextOutput("fileob")
        )
    )
))
