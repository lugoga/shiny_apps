library(shiny)
library(bslib)
library(htmltools)
library(leaflet)
library(dplyr)

# Custom CSS for floating animation and widget styling
custom_css <- "
@keyframes float {
  0% { transform: translateY(0px); }
  50% { transform: translateY(-10px); }
  100% { transform: translateY(0px); }
}

.weather-widget {
  background: linear-gradient(135deg, #6BA5F2, #3778C7);
  border-radius: 15px;
  color: white;
  padding: 20px;
  box-shadow: 0 10px 20px rgba(0,0,0,0.1);
}

.weather-icon {
  animation: float 4s ease-in-out infinite;
  font-size: 4rem;
  margin: 10px;
  text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
}

.weather-details {
  font-size: 1.2rem;
  margin-top: 15px;
}

.location-input {
  background: rgba(255,255,255,0.2);
  border: none;
  border-radius: 25px;
  padding: 10px 20px;
  color: white;
  margin-bottom: 15px;
}

.temp-unit-toggle .btn {
  background: rgba(255,255,255,0.2);
  border: none;
  color: white;
}

.temp-unit-toggle .btn.active {
  background: rgba(255,255,255,0.4);
  font-weight: bold;
}
"

# Tanzania regions with coordinates
tanzania_regions <- tribble(
  ~region, ~lat, ~lng,
  "Arusha", -3.3667, 36.6833,
  "Dar es Salaam", -6.8000, 39.2833,
  "Dodoma", -6.1730, 35.7419,
  "Geita", -2.8727, 32.2359,
  "Iringa", -7.7700, 35.7000,
  "Kagera", -1.8643, 31.6167,
  "Katavi", -6.9000, 31.5000,
  "Kigoma", -4.8769, 29.6267,
  "Kilimanjaro", -3.0674, 37.3556,
  "Lindi", -9.9959, 39.7151,
  "Manyara", -3.5984, 36.7963,
  "Mara", -1.9437, 33.8062,
  "Mbeya", -8.9000, 33.4500,
  "Morogoro", -6.8219, 37.6597,
  "Mtwara", -10.2667, 40.1833,
  "Mwanza", -2.5167, 32.9000,
  "Njombe", -9.3298, 34.7714,
  "Pemba", -5.0667, 39.7833,
  "Pwani", -7.0000, 38.5000,
  "Rukwa", -5.9484, 31.6682,
  "Ruvuma", -10.6867, 35.6500,
  "Shinyanga", -3.6617, 33.4271,
  "Simiyu", -2.8300, 34.7000,
  "Singida", -4.8186, 34.7506,
  "Tabora", -5.0167, 32.8000,
  "Tanga", -5.0667, 39.1000,
  "Zanzibar", -6.1667, 39.2000
)

ui <- page_sidebar(
  tags$head(
    tags$link(rel = "stylesheet", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/weather-icons/2.0.10/css/weather-icons.min.css"),
    tags$style(custom_css)
  ),
  
  sidebar = sidebar(
    width = "300px",
    # Weather Widget
    card(
      class = "weather-widget",
      
      # Region selector
      selectInput("location", "Select Region",
                  choices = tanzania_regions$region,
                  selected = "Dar es Salaam",
                  width = "100%",
                  selectize = TRUE),
      
      # Temperature unit toggle
      div(class = "temp-unit-toggle mb-4",
          radioButtons("temp_unit", NULL,
                       choices = c("°C", "°F"),
                       selected = "°C",
                       inline = TRUE)),
      
      # Current weather display
      div(class = "text-center",
          uiOutput("weather_icon"),
          h2(textOutput("temperature"), class = "mb-3"),
          h3(textOutput("weather_desc"), class = "mb-4")),
      
      # Weather details
      div(class = "weather-details",
          layout_column_wrap(
            width = 1/2,
            div(
              tags$i(class = "wi wi-humidity mr-2"),
              "Humidity: ",
              textOutput("humidity", inline = TRUE)
            ),
            div(
              tags$i(class = "wi wi-strong-wind mr-2"),
              "Wind: ",
              textOutput("wind", inline = TRUE)
            )
          ),
          layout_column_wrap(
            width = 1/2,
            div(
              tags$i(class = "wi wi-raindrop mr-2"),
              "Rainfall: ",
              textOutput("rainfall", inline = TRUE)
            ),
            div(
              tags$i(class = "wi wi-sunrise mr-2"),
              "Sunrise: ",
              textOutput("sunrise", inline = TRUE)
            )
          )
      )
    ),
  ),
  
  
  layout_column_wrap(
    width = 1/1,
  
    
    # Map Card
    card(
      height = "600px",
      leafletOutput("map", height = "100%")
    )
  )
)

server <- function(input, output, session) {
  # Simulated weather data for Tanzania regions
  weather_data <- reactive({
    # Simulate different weather patterns based on region
    is_coastal <- input$location %in% c("Dar es Salaam", "Tanga", "Pwani", "Mtwara", "Lindi")
    is_highland <- input$location %in% c("Kilimanjaro", "Arusha", "Mbeya", "Iringa")
    
    # Adjust temperature ranges based on region type
    temp_range <- if(is_coastal) {
      25:32
    } else if(is_highland) {
      15:25
    } else {
      20:30
    }
    
    # Adjust humidity based on region type
    humidity_range <- if(is_coastal) {
      70:85
    } else if(is_highland) {
      50:65
    } else {
      55:75
    }
    
    list(
      temp_c = sample(temp_range, 1),
      humidity = sample(humidity_range, 1),
      wind_speed = sample(5:25, 1),
      rainfall = sample(0:100, 1),
      sunrise = "6:15 AM"
    )
  })
  
  # Weather description based on region characteristics
  weather_description <- reactive({
    if(input$location %in% c("Dar es Salaam", "Tanga", "Pwani")) {
      sample(c("Partly Cloudy", "Humid", "Tropical Shower"), 1)
    } else if(input$location %in% c("Kilimanjaro", "Arusha")) {
      sample(c("Clear Sky", "Mountain Clouds", "Cool Breeze"), 1)
    } else {
      sample(c("Sunny", "Scattered Clouds", "Light Breeze"), 1)
    }
  })
  
  # Convert temperature based on selected unit
  output$temperature <- renderText({
    temp_c <- weather_data()$temp_c
    if(input$temp_unit == "°F") {
      sprintf("%.1f°F", (temp_c * 9/5) + 32)
    } else {
      sprintf("%.1f°C", temp_c)
    }
  })
  
  # Weather icon (changes based on region and conditions)
  output$weather_icon <- renderUI({
    icon_class <- if(input$location %in% c("Dar es Salaam", "Tanga", "Pwani")) {
      sample(c("wi-day-cloudy", "wi-hot", "wi-day-rain"), 1)
    } else if(input$location %in% c("Kilimanjaro", "Arusha")) {
      sample(c("wi-day-sunny", "wi-cloudy", "wi-day-cloudy"), 1)
    } else {
      sample(c("wi-day-sunny", "wi-day-sunny-overcast", "wi-day-light-wind"), 1)
    }
    
    tags$i(class = paste("wi", icon_class, "weather-icon"))
  })
  
  output$weather_desc <- renderText({
    weather_description()
  })
  
  output$humidity <- renderText({
    paste0(weather_data()$humidity, "%")
  })
  
  output$wind <- renderText({
    paste0(weather_data()$wind_speed, " km/h")
  })
  
  output$rainfall <- renderText({
    paste0(weather_data()$rainfall, " mm")
  })
  
  output$sunrise <- renderText({
    weather_data()$sunrise
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 35.7419, lat = -6.1730, zoom = 6) %>%
      addCircleMarkers(
        data = tanzania_regions,
        lng = ~lng,
        lat = ~lat,
        popup = ~region,
        radius = 8,
        color = "#3778C7",
        fillOpacity = 0.8,
        weight = 2
      )
  })
  
  # Update map when region is selected
  observe({
    selected_region <- tanzania_regions %>%
      filter(region == input$location)
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addCircleMarkers(
        data = selected_region,
        lng = ~lng,
        lat = ~lat,
        radius = 12,
        color = "red",
        fillOpacity = 1,
        weight = 3,
        group = "highlight"
      ) %>%
      setView(
        lng = selected_region$lng,
        lat = selected_region$lat,
        zoom = 8
      )
  })
  
  # Update data every 30 seconds
  observe({
    invalidateLater(30000)
    weather_data()
  })
}

shinyApp(ui, server)
