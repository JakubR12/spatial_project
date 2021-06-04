library(shiny)
library(leaflet)
library(mapview)
library(tidyverse)
library(colourpicker)
library(leafpop)
library(shinyWidgets)
library(colourpicker)
library(varhandle)
library(sp)

alert <- function(title, text, type) {
  shinyalert(
    title = title,
    text = text,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = type,
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
}
1

error_check <- function(df, label_col) {
  #This function checks that the data is in right format
  
  len = length(df[, 1])
  
  if (!sum(c("lat", "long") %in% colnames(df)) == 2) {
    alert(title = "Separator Error !!",
          text = "You don't seem to have to have coordinates in columns called 'lat' and 'long'. You may have chosen a wrong seperator. You can see your data set below the map and choose the right separator.",
          type = "error")
    
  }
  else if (!sum(check.numeric(df$long) + check.numeric(df$lat)) == len * 2) {
    alert(title = "Longitude or Latitude Class Error !!",
          text = "It seems that some of your values in the long or lat columns are not numeric. Double check their formats.",
          type = "warning")
  }
  else if (!sum(df$long <= 180 & df$long >= -180) == len) {
    alert(title = "Longitude Error !!",
          text = "It seems that your long column contains a value outside of the allowed range for longitude (-180 - 180). Ensure that all values are within the range.",
          type = "error")
  }
  
  else if (!sum(df$lat <= 90 & df$lat >= -90) == len) {
    alert(title = "Latitude Error !!",
          text = "It seems that your lat column contains a value outside of the allowed range for latitude (-90 - 90). More often that not, this is caused by flipping long and lat columns. Ensure that all values are within the range.",
          type = "error")
  }
  else if (ncol(select_if(df, names(df) %in% label_col)) != 1) {
    alert(title = "Label Error !!",
          text = "It seems that your column name for labelling/describing locations does not correspond to the default: Description. You can change it in the Upload options in the Label name box.",
          type = "error")
  }
  else {
    alert(title = "Success !!",
          text = "Your data is loaded in the right format, now you can customize the aesthetics!",
          type = "success")
  }
}

