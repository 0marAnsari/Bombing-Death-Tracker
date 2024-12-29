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
library(httr)  # For handling API requests
library(jsonlite) # For handling the response of the API
library(dplyr) # For handling data

# Base URL
base_url <- "https://api.acleddata.com/acled/read"

# Set up the list of parameters
params <- list(
  email = "oansari@uchicago.edu",
  key = "IbF5sT9lJbY-7eqg7LFf",  # Replace with your actual API key
  country = "Israel|Palestine|Syria|Lebanon", 
  fields = "event_date|year|latitude|longitude|event_type|sub_event_type|country|fatalities|actor1|actor2",
  limit = 5000,  # Maximum records per page
  page = 1       # Starting page
)

# Initialize an empty list to store all records
all_data <- list()

# Paginated request loop for 2023 and 2024
for (yr in c(2023, 2024)) {
  cat("Fetching data for year", yr, "...\n")
  params$year <- yr  # Add the year filter to the parameters
  params$page <- 1   # Reset the page to 1 for each year
  
  repeat {
    cat("Fetching page", params$page, "for year", yr, "...\n")
    
    # Make the API request
    response <- GET(url = base_url, query = params)
    
    # Check response status
    if (status_code(response) != 200) {
      stop("API request failed with status:", status_code(response))
    }
    
    # Parse the JSON response
    response_json <- jsonlite::fromJSON(content(response, "text"), simplifyVector = TRUE)
    
    # Append data to the list
    if (!is.null(response_json$data) && length(response_json$data) > 0) {
      # Convert to a DataFrame and add the year column
      df <- as.data.frame(response_json$data)
      df$year <- yr
      all_data <- append(all_data, list(df))
      params$page <- params$page + 1  # Increment the page number
    } else {
      cat("No more data available for year", yr, ".\n")
      break  # Exit loop if no more data
    }
  }
}

# Combine all pages into a single data frame
final_data <- bind_rows(all_data)

# Save the data to a CSV file
write.csv(final_data, "acled_2023_2024_data.csv", row.names = FALSE)
cat("Data saved to 'acled_2023_2024_data.csv'.\n")

library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(sf)
library(fastmap)
library(bslib)
library(leaflet.extras)
library(ggplot2)
library(shinyWidgets)

# Load your ACLED data
acled_data <- read.csv("acled_2023_2024_data.csv")
acled_data$event_date <- as.Date(acled_data$event_date)

ui <- fluidPage(
  titlePanel("Dynamic Event Density Map for Israel, Palestine, Lebanon, and Syria"),
  sidebarLayout(
    sidebarPanel(
      switchInput(
        "mode",
        label = "Mode",
        onLabel = "Animate",
        offLabel = "Manual",
        value = FALSE,  # Default to manual
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
  
  # Reactive filtered data for animation mode
  animation_data <- reactive({
    acled_data %>%
      filter(event_date == input$animation_date) %>%
      filter(event_type == "Explosions/Remote violence" & actor1 == "Military Forces of Israel (2022-)") %>%
      filter(country %in% c("Israel", "Palestine", "Syria", "Lebanon"))
  })
  
  # Reactive fatalities for counters
  cumulative_data <- reactive({
    if (input$mode == FALSE) {  # Manual mode
      acled_data %>%
        filter(event_date >= input$date_range[1] & event_date <= input$date_range[2]) %>%
        filter(event_type == "Explosions/Remote violence" & actor1 == "Military Forces of Israel (2022-)") %>%
        filter(country %in% c("Israel", "Palestine", "Syria", "Lebanon")) %>%
        group_by(country) %>%
        summarize(cumulative_fatalities = sum(fatalities, na.rm = TRUE))
    } else {  # Animation mode
      acled_data %>%
        filter(event_date <= input$animation_date) %>%
        filter(event_type == "Explosions/Remote violence" & actor1 == "Military Forces of Israel (2022-)") %>%
        filter(country %in% c("Israel", "Palestine", "Syria", "Lebanon")) %>%
        group_by(country) %>%
        summarize(cumulative_fatalities = sum(fatalities, na.rm = TRUE))
    }
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
    
    israel_fatalities <- if ("Israel" %in% cumulative$country) cumulative %>% filter(country == "Israel") %>% pull(cumulative_fatalities) else 0
    palestine_fatalities <- if ("Palestine" %in% cumulative$country) cumulative %>% filter(country == "Palestine") %>% pull(cumulative_fatalities) else 0
    syria_fatalities <- if ("Syria" %in% cumulative$country) cumulative %>% filter(country == "Syria") %>% pull(cumulative_fatalities) else 0
    lebanon_fatalities <- if ("Lebanon" %in% cumulative$country) cumulative %>% filter(country == "Lebanon") %>% pull(cumulative_fatalities) else 0
    
    tags$div(
      style = "text-align: center; margin-top: 20px;",
      tags$div(style = "font-size: 20px; font-weight: bold; color: #333;", "Fatalities by Country Caused by Israel:"),
      tags$div(style = "font-size: 18px; color: #333;", paste("Israel: ", israel_fatalities)),
      tags$div(style = "font-size: 18px; color: #333;", paste("Palestine: ", palestine_fatalities)),
      tags$div(style = "font-size: 18px; color: #333;", paste("Syria: ", syria_fatalities)),
      tags$div(style = "font-size: 18px; color: #333;", paste("Lebanon: ", lebanon_fatalities))
    )
  })
}

shinyApp(ui = ui, server = server)