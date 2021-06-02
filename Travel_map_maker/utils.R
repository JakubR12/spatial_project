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

# EPSG 54042, SR-ORG:7291

error_check <- function(df, label_col) {
  len = length(df[, 1])
  
  if (!sum(c("lat", "long") %in% colnames(df)) == 2) {
    alert(title = "Error !!",
          text = "it seems that you have uchosen the wrong seperator. Try and check the box with the right seperator for your data",
          type = "error")
    
  }
  else if (!sum(check.numeric(df$long) + check.numeric(df$lat)) == len * 2) {
    alert(title = "Error !!",
          text = "not numeric",
          type = "warning")
  }
  else if (!sum(df$long <= 180 & df$long >= -180) == len) {
    alert(title = "Error !!",
          text = "longitude error",
          type = "error")
  }
  
  else if (!sum(df$lat <= 90 & df$lat >= -90) == len) {
    alert(title = "Error !!",
          text = "latitude error",
          type = "error")
  }
  else if (ncol(select_if(df, names(df) %in% label_col)) != 1) {
    alert(title = "Error !!",
          text = "Label Name Error",
          type = "error")
  }
  else {
    alert(title = "Success !!",
          text = "All in order",
          type = "success")
  }
}
