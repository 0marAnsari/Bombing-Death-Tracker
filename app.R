#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Load your ACLED data
acled_data <- read.csv("acled_2023_2024_data.csv")
acled_data$event_date <- as.Date(acled_data$event_date)


library(shiny)
library(leaflet)
library(lubridate)
library(sf)
library(fastmap)
library(bslib)
library(leaflet.extras)
library(ggplot2)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Dynamic Bombing Density Map for Israel, Palestine, Lebanon, and Syria"),
  sidebarLayout(
    sidebarPanel(
      switchInput(
        "mode",
        label = "Mode",
        onLabel = "Animate",
        offLabel = "Manual",
        value = TRUE,  # Default to animation
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.mode == false",  # Manual mode
        sliderInput(
          "date_range", 
          "Select Date Range:",
          min = as.Date("2023-10-01"),
          max = max(acled_data$event_date),
          value = c(as.Date("2023-10-01"), max(acled_data$event_date)),
          timeFormat = "%Y-%m-%d"
        )
      ),
      conditionalPanel(
        condition = "input.mode == true",  # Animation mode
        sliderInput(
          "animation_date", 
          "Animate Through Dates:",
          min = as.Date("2023-10-01"),
          max = max(acled_data$event_date),
          value = as.Date("2023-10-01"),
          timeFormat = "%Y-%m-%d",
          animate = animationOptions(interval = 167, loop = TRUE)  # Animation settings
        )
      )
    ),
    mainPanel(
      leafletOutput("event_map", height = "630px"),
      htmlOutput("fatality_counters")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data for manual mode
  manual_data <- reactive({
    acled_data %>%
      filter(event_date >= input$date_range[1] & event_date <= input$date_range[2]) %>%
      filter(event_type == "Explosions/Remote violence" & actor1 == "Military Forces of Israel (2022-)") %>%
      filter(country %in% c("Israel", "Palestine", "Syria", "Lebanon"))
  })
  
  # Reactive filtered data for cumulative animation mode
  animation_data <- reactive({
    acled_data %>%
      filter(event_date >= as.Date("2023-10-01") & event_date <= input$animation_date) %>%
      filter(event_type == "Explosions/Remote violence" & actor1 == "Military Forces of Israel (2022-)") %>%
      filter(country %in% c("Israel", "Palestine", "Syria", "Lebanon"))
  })
  
  # Reactive fatalities for counters
  cumulative_data <- reactive({
    if (input$mode == FALSE) {  # Manual mode
      manual_data() %>%
        group_by(country) %>%
        summarize(cumulative_fatalities = sum(fatalities, na.rm = TRUE))
    } else {  # Animation mode
      animation_data() %>%
        group_by(country) %>%
        summarize(cumulative_fatalities = sum(fatalities, na.rm = TRUE))
    }
  })
  
  # Reactive fatalities for Hamas and Hezbollah
  group_fatalities <- reactive({
    acled_data %>%
      filter(event_date >= as.Date("2023-10-01") & event_date <= if (input$mode == FALSE) max(input$date_range) else input$animation_date) %>%
      filter(actor1 %in% c("Hamas Movement", "Hezbollah")) %>%
      group_by(actor1) %>%
      summarize(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
      tidyr::pivot_wider(names_from = actor1, values_from = total_fatalities, values_fill = 0)
  })
  
  # Render leaflet map
  output$event_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 35.2137, lat = 31.7683, zoom = 8) %>%
      addProviderTiles("CartoDB.Positron")
  })
  
  # Update heatmap based on selected mode
  observe({
    filtered_data <- if (input$mode == FALSE) manual_data() else animation_data()
    
    leafletProxy("event_map", data = filtered_data) %>%
      clearHeatmap() %>%
      addHeatmap(
        lat = ~latitude,
        lng = ~longitude,
        intensity = ~1,
        blur = 20,
        max = 0.05,
        radius = 15
      )
  })
  
  # Update fatality counters
  output$fatality_counters <- renderUI({
    cumulative <- cumulative_data()
    group_data <- group_fatalities()
    
    israel_fatalities <- if ("Israel" %in% cumulative$country) cumulative %>% filter(country == "Israel") %>% pull(cumulative_fatalities) else 0
    palestine_fatalities <- if ("Palestine" %in% cumulative$country) cumulative %>% filter(country == "Palestine") %>% pull(cumulative_fatalities) else 0
    syria_fatalities <- if ("Syria" %in% cumulative$country) cumulative %>% filter(country == "Syria") %>% pull(cumulative_fatalities) else 0
    lebanon_fatalities <- if ("Lebanon" %in% cumulative$country) cumulative %>% filter(country == "Lebanon") %>% pull(cumulative_fatalities) else 0
    
    hamas_fatalities <- if ("Hamas Movement" %in% colnames(group_data)) group_data$`Hamas Movement` else 0
    hezbollah_fatalities <- if ("Hezbollah" %in% colnames(group_data)) group_data$`Hezbollah` else 0
    
    tags$div(
      style = "text-align: center; margin-top: 20px;",
      tags$div(style = "font-size: 20px; font-weight: bold; color: #333;", "Fatalities by Country Caused by Israel:"),
      tags$div(style = "font-size: 18px; color: #333;", paste("Israel: ", israel_fatalities)),
      tags$div(style = "font-size: 18px; color: #333;", paste("Palestine: ", palestine_fatalities)),
      tags$div(style = "font-size: 18px; color: #333;", paste("Syria: ", syria_fatalities)),
      tags$div(style = "font-size: 18px; color: #333;", paste("Lebanon: ", lebanon_fatalities)),
      tags$hr(),
      tags$div(style = "font-size: 20px; font-weight: bold; color: #333;", "Fatalities Caused by Specific Groups:"),
      tags$div(style = "font-size: 18px; color: #333;", paste("Hamas Movement: ", hamas_fatalities)),
      tags$div(style = "font-size: 18px; color: #333;", paste("Hezbollah: ", hezbollah_fatalities))
    )
  })
}


shinyApp(ui = ui, server = server)

