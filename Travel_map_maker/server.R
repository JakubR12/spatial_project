library(shiny) # For running the app
library(tidyverse) #for data wrangling
library(shinyalert) #For error pop ups
library(leaflet) #For making the map
library(mapview) #for saving the map
library(colourpicker) #For color selection in the app
library(leafpop) #For image pop ups
library(varhandle) #for checking numeric values
library(sp) #For converting dms values to numeric

source("utils.R") #sourcing custom functions

options(shiny.maxRequestSize = 30 * 1024 ^ 2)

#define server logic
shinyServer(function(input, output) {
  #the csv loader
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return()
    }
    
    if (input$dms == T) {
      #read data
      df <-
        read.csv(infile$datapath, sep = input$seperator)
      #if it does not have the longitude columns, it can't convert the values
      if (is.null(df$long)) {
        return(df)
      }
      #convert to numeric
      else if (is.numeric(df$lat) & is.numeric(df$lat)) {
        return(df)
      }
      else {
        #parse dms and convert to decimal degrees
        df = df %>% mutate(lat =
                             as.numeric(char2dms(
                               df$lat, input$chd, input$chm, input$chs
                             )),
                           long =
                             as.numeric(char2dms(
                               df$long, input$chd, input$chm, input$chs
                             )))
      } <
        return(df)
    }
    #just load data normally
    data <-
      read.csv(infile$datapath, sep = input$seperator) #infile$datapath
    return(data)
    
    
  })
  #creating a reactive value for the customizable describtion column name
  description <- reactive({
    label <- input$description
    if (is.null(label)) {
      return("description")
    }
    else {
      return(label)
    }
  })
  
  #Diagnose error button
  observeEvent(input$diagnose, {
    if (is.null(input$file1)) {
      return ()
    }
    df = filedata()
    label_col = description()
    error_check(df, label_col)
  })
  
  #Monitor for errors when uploading data
  observeEvent(input$file1, {
    if (is.null(input$file1)) {
      return ()
    }
    df = filedata()
    label_col = description()
    error_check(df, label_col)
  })
  
  #reactive value for approving the data format
  format_approved <- reactive ({
    if (is.null(input$file1)) {
      return ()
    }
    df = filedata()
    len = length(df[, 1])
    label_col = description()
    
    if (sum(c("lat", "long", label_col) %in% colnames(df)) == 3 &
        sum(check.numeric(df$long) + check.numeric(df$lat)) == len * 2 &
        sum(df$long <= 180 &
            df$long >= -180) == len &
        sum(df$lat <= 90 & df$lat >= -90) == len) {
      TRUE
    }
    
    else {
      FALSE
    }
  })
  
  #reactive value that can handle the relative file paths for the images
  merged_df <- reactive({
    if (is.null(input$file1)) {
      return ()
    }
    df = filedata()
    if (!is.null(input$file2)) {
      df = input$file2 %>%
        rename(Image_name = name) %>%
        merge(df, by = "Image_name")
    }
    df
  })
  
  #The file structure output
  output$fileob <- renderPrint({
    if (is.null(input$file1)) {
      return ("No data has been uploaded yet")
    }
    df = filedata()
    str(df)
    
  })
  
  #creating interactive table for the data
  output$contents = renderDataTable({
    if (is.null(input$file1)) {
      return ()
    }
    df = filedata()
    return(df)
  })
  
  #reactivebject for the leaflet map
  main_map <- reactive({
    #call the dataframe with the full image paths
    df <-  merged_df()
    #initiate map
    start_map <- leaflet()
    #grab all esri tile layers
    esri <- grep("^Esri", providers, value = TRUE)
    #add esri tile layers
    for (provider in esri) {
      start_map <-
        start_map %>% addProviderTiles(provider,
                                       group = provider,
                                       options = providerTileOptions(noWrap = !input$wrap_map))
    }
    
    #use html code to format the title
    map_title <-
      paste(
        "<b style='color:",
        input$title_color,
        ";font-size:",
        input$title_size
        ,
        "px;font-family:Comic Sans MS'>",
        #Comic Sans FTW
        input$map_title
        ,
        "<b/>",
        sep = ""
      )
    
    #create the basic map without data
    plot <- start_map %>%
      
      addLayersControl(baseGroups = names(esri),
                       options = layersControlOptions(collapsed = T))  %>%
      addMiniMap(tiles = esri[[1]],
                 toggleDisplay = TRUE,
                 position = "bottomright") %>%
      addMeasure(
        position = "bottomright",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      ) %>%
      addControl(map_title, "bottomleft")
    
    #use base map if no data has been uploaded
    if (is.null(input$file1)) {
      return(plot)
    }
    #Use the map with markers if the data has been uploaded and approved
    if (req(format_approved()) == TRUE) {
      label_col <-  description()
      label_df <-  select_if(df, names(df) %in% label_col)
      if (ncol(label_df) == 0) {
        label_name <- NULL
      } else{
        label_name <- names(label_df)
      }
      
      icons <- makeAwesomeIcon(
        text = fa(input$icon),
        markerColor = input$markerColor,
        iconColor = input$icon_color,
        spin = input$icon_spin,
        squareMarker = input$square_markers
      )
      
      if(input$html == T){html_labels <- lapply(df[label_name][, 1],htmltools::HTML)}
      else{html_labels <- df[label_name][, 1] }
      
      
      marker_plot <- plot %>%
        addAwesomeMarkers(
          data = df,
          lng = df$long,
          lat = df$lat,
          label = html_labels,
          icon = icons,
          group = "pnts",
          #clusterOptions = markerClusterOptions(),
          options = markerOptions(opacity = 0.8)
        )
    }
    #add images if imges has been uploaded
    if (!is.null(df) & !is.null(df$datapath)) {
      return(marker_plot %>%
               addPopupImages(df$datapath, "pnts", 150))
    }
    #else return map just with markers
    else if (req(format_approved()) == TRUE) {
      return(marker_plot)
    } #otherwise just return the base map
    else {
      return(plot)
    }
  })
  #render the map
  output$map <- leaflet::renderLeaflet({
    main_map()
  })
  #reactive object for saving the state of the map
  mymap <- reactive({
    # call the foundational Leaflet map
    main_map() %>%
      # store the view based on UI
      setView(
        lng = input$map_center$lng
        ,
        lat = input$map_center$lat
        ,
        zoom = input$map_zoom
      ) %>%
      showGroup(group = input$map_groups)
    
  })
  #download map as png saved as the state of the map
  output$downloadPlotPNG <- downloadHandler(
    filename = "data.png"
    ,
    
    content = function(file) {
      mapshot(
        x = mymap(),
        file = file,
        cliprect = "viewport",
        selfcontained = T
      )
    }
    
  )
  #save html version of the map. The map should be self contained in the html to work wethink...
  output$downloadPlotHTML <- downloadHandler(
    filename = paste0(getwd(), "/map.html")
    ,
    
    content = function(file) {
      mapshot(
        x = mymap(),
        cliprect = "viewport",
        selfcontained = T,
        url = file
      )
    }
    
  )
})