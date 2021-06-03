library(shiny) #for running the app
library(leaflet) #for making the map
library(fontawesome) #for awesome fonts
library(shinythemes) #app theme
library(colourpicker) #for colour picker widget
#library(shinyBS)
library(shinyalert)#for errer alerts
library(shinyWidgets) # added ui fuctionality

#list of marker colors
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

# Define UI
shinyUI(
    fluidPage(
    useShinyalert(),
    #set theme
    theme = shinytheme("readable"),
    # Application title
    titlePanel("Travel Map Maker Tool"),
    
    # layout
    sidebarLayout(
        sidebarPanel(
            "Welcome to the traval map maker tool! This tool lets you create and customize interactive maps of whre you have traveled and lets you share the result with your friends and family.",
            #wrap some upload stuff in a drowpdown menu
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
                textAreaInput("description", "Label Column", "Description"),
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
            #menu for converting dms to dd
            dropdownButton(
                textInput("chd", "Day seperator", "d"),
                textInput("chm", "Minute seperator", "'"),
                textInput("chs", "Second seperator", "\""),
                checkboxInput("dms", "DMS", F),
                circle = FALSE,
                label = "DMS Conversion Options"
            ),
            #button to activat the error_check function
            actionButton(
                inputId = "diagnose",
                label = "Diagnose problems",
                icon = icon("remove")
            ),
            br(),
            br(),
            # Icons in dorpdown menu
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
                checkboxInput("html", "Render Labels as HTML", T),
                circle = FALSE
            ),
            #title editiing in dropdown menu
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
            br(),
            # download buttons
            dropdownButton(
                downloadButton("downloadPlotPNG", "Download PNG"),
                downloadButton("downloadPlotHTML", "Download HTML"),
                circle = FALSE,
                label = "Download Options",
                icon = icon("download")
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map", height = 600),
            dataTableOutput("contents"),
            verbatimTextOutput("fileob")
        )
    )
))