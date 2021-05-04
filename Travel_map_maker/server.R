#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(mapview)
library(tidyverse)
library(colourpicker)
library(leafpop)
library(shinyWidgets)
library(colourpicker)

# EPSG 54042, SR-ORG:7291

addTitle = function(object,
                    text,
                    color = "black",
                    fontSize = "20px",
                    fontFamily = "Sans",
                    leftPosition = 50,
                    topPosition = 2) {
  htmlwidgets::onRender(
    object,
    paste0(
      "
                                       function(el,x){
                                       h1 = document.createElement('h1');
                                       h1.innerHTML = '",
      text ,
      "';
                                       h1.id='titleh1';
                                       h1.style.color = '",
      color ,
      "';
                                       h1.style.fontSize = '",
      fontSize,
      "';
                                       h1.style.fontFamily='",
      fontFamily,
      "';
                                       h1.style.position = 'fixed';
                                       h1.style['-webkit-transform']='translateX(-50%)';
                                       h1.style.left='",
      leftPosition ,
      "%';
                                       h1.style.top='",
      topPosition,
      "%';
                                       document.body.appendChild(h1);
                                       }"
    )
  )
  
}
options(shiny.maxRequestSize=30*1024^2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath, sep = input$seperator) #infile$datapath
  })
  
  
  
  # Defensive coding
  ## wrong decimal
  # output$coor_range <- renderPrint({
  #     dch = filedata()
  #     
  #     
  #     grepl( "\\.",dch$long | dch$lat)
  #     print("sansa")
  #     
  #   })
  # })
  output$coor_range <- renderText({
    if(is.null(input$file1)){return ()}
    dch = filedata()
    #ifelse(dch$long < 90 & dch$long> -90, "\n Well done", "Wrong format")
    # if ((min(as.numeric(dch$long)) < -90 | max(as.numeric(dch$long)) > 90) | 
    #     (min(dch$lat) < -180 | max(dch$lat) > 180)){
    #   "Check the data format"
    # } else ("Looks good.")
    
    print(head(dch))
  })
  
   observeEvent(input$success, {
    show_alert(
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
  })

  observeEvent(input$error, {
    show_alert(
      title = "Error !!",
      text = "it seems that you have uploaded your coordinates in the wrong format :/ The coordinates needs to correspond to the WGS84 CRS and be denoted  in decimal format with '.' as decimal delimiter.",
      type = "error"
    )
  })


  
  merged_df <- reactive({
    if(is.null(input$file2)){return ()}
    if(is.null(input$file1)){return ()}
    df = filedata()
    input$file2 %>% 
      rename(Image_name = name) %>% 
      merge(df, by = "Image_name")
  })
  
  
  output$fileob <- renderPrint({
    if(is.null(input$file2)){return ()}
    if(is.null(input$file1)){return ()}
    df = filedata()
    # input$file2 %>% 
    #   rename(Image_name = name) %>% 
    #   merge(df, by = "Image_name")
    str(df)
    
    #class(input$file2)

  })
  
  image_info <- reactive({
    if(is.null(input$file2)){return ()}
    if(is.null(input$file1)){return ()}
    df = filedata()
    input$file2 %>%
      rename(Image_name = name) %>%
      merge(df, by = "Image_name")
  })
  output$contents = renderDataTable({
    if(is.null(input$file2)){return ()}
    if(is.null(input$file1)){return ()}
    df = filedata()
    return(df)
  })
  
  
  
  main_map <- reactive({
    df = merged_df()
    
    start_map <- leaflet()
    
    esri <- grep("^Esri", providers, value = TRUE)
    
    for (provider in esri) {
      start_map <-
        start_map %>% addProviderTiles(provider,
                                       group = provider,
                                       options = providerTileOptions(noWrap = !input$wrap_map))
    }
    map_title <-
      paste(
        "<b style='color:",input$title_color,";font-size:",
        input$title_size
        ,
        "px;font-family:Comic Sans MS'>",
        input$map_title
        ,
        "<b/>",
        sep = ""
      )
    
    
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
    
    
    if (!is.null(df)) {
      
      icons <- makeAwesomeIcon(
        text = fa(input$icon),
        markerColor = input$markerColor,
        iconColor = input$icon_color,
        spin = input$icon_spin,
        squareMarker = input$square_markers
      )
      
      plot %>%
        addAwesomeMarkers(
          data = df,
          lng = df$long,
          lat = df$lat,
          label = df$Description,
          icon = icons,
          group = "pnts",
          #clusterOptions = markerClusterOptions(),
          options = markerOptions(opacity = 0.8)
        ) %>%
        addPopupImages(df$datapath, "pnts", 150)
    } else {
      plot
    }
  })
  
  output$map <- leaflet::renderLeaflet({
    main_map()
    
  })

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
  
  output$downloadPlotPNG <- downloadHandler(
    filename = "data.png"
    ,
    
    content = function(file) {
      mapshot(
        x = mymap(),
        file = file,
        cliprect = "viewport",
        selfcontained =T
      )
    }
    
  )
  
  output$downloadPlotHTML <- downloadHandler(
    filename = paste0(getwd(), "/map.html")
    ,
    
    content = function(file) {
      mapshot(
        x = mymap(),
        #file = file,
        cliprect = "viewport",
        selfcontained =T, 
        url = file
      )
    }
    
  )
  renderPrint({
    if(is.null(df)){return ()}
    df$Image_name
  })
  
})
