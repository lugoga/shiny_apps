library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)

# Simulate additional data
set.seed(123)
fishing_data <- data.frame(
  date = seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by='day'),
  species = sample(c('Nile Perch', 'Dagaa', 'Tilapia', 'Catfish'), 365, replace = TRUE),
  catch_kg = rnorm(365, mean = 500, sd = 100),
  location = sample(c('Lake Victoria', 'Lake Tanganyika', 'Indian Ocean', 'Lake Nyasa'), 365, replace = TRUE),
  lat = runif(365, -11.7, -1.0),
  lon = runif(365, 29.3, 40.4),
  vessel_id = sample(paste0("TZ-", 1:10), 365, replace = TRUE),
  price_per_kg = rnorm(365, mean = 5, sd = 1),
  weather_condition = sample(c("Sunny", "Cloudy", "Rainy"), 365, replace = TRUE),
  quota_limit = 600  # Daily quota limit in kg
)

# Simulate vessel data
vessel_data <- data.frame(
  vessel_id = paste0("TZ-", 1:10),
  vessel_name = paste("Vessel", LETTERS[1:10]),
  capacity = runif(10, 1000, 5000),
  last_maintenance = sample(seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by='day'), 10)
)

ui <- page_navbar(
  title = "Tanzania Fisheries Management System",
  theme = bs_theme(version = 5, bootswatch = "united"),
  
  nav_panel("Dashboard",
            page_sidebar(
              sidebar = sidebar(
                selectInput("location", "Select Fishing Location:",
                            choices = unique(fishing_data$location)),
                selectInput("species", "Select Species:",
                            choices = unique(fishing_data$species)),
                dateRangeInput("date_range", "Select Date Range:",
                               start = min(fishing_data$date),
                               end = max(fishing_data$date)),
                downloadButton("download_report", "Export Report"),
                hr(),
                helpText("This system helps monitor and analyze fishing activities.")
              ),
              
              layout_columns(
                fill = FALSE,
                value_box(
                  title = "Total Catch",
                  value = textOutput("total_catch"),
                  showcase = bsicons::bs_icon("water"),
                  theme_color = "primary"
                ),
                value_box(
                  title = "Daily Average",
                  value = textOutput("daily_avg"),
                  showcase = bsicons::bs_icon("graph-up"),
                  theme_color = "secondary"
                ),
                value_box(
                  title = "Quota Status",
                  value = textOutput("quota_status"),
                  showcase = bsicons::bs_icon("clipboard-data"),
                  theme_color = if (sum(fishing_data$catch_kg) > fishing_data$quota_limit[1]) "danger" else "success"
                )
              ),
              
              layout_columns(
                card(
                  card_header("Catch Trends Over Time"),
                  plotOutput("trend_plot")
                ),
                card(
                  card_header("Fishing Locations"),
                  leafletOutput("map", height = 400)
                )
              )
            )
  ),
  
  nav_panel("Statistical Analysis",
            layout_columns(
              card(
                card_header("Statistical Summary"),
                selectInput("analysis_var", "Select Variable:",
                            choices = c("Catch (kg)" = "catch_kg", 
                                        "Price per kg" = "price_per_kg")),
                verbatimTextOutput("summary_stats")
              ),
              card(
                card_header("Distribution Analysis"),
                plotOutput("dist_plot")
              )
            )
  ),
  
  nav_panel("Weather Integration",
            layout_columns(
              card(
                card_header("Weather Conditions"),
                selectInput("weather_location", "Select Location:",
                            choices = unique(fishing_data$location)),
                plotOutput("weather_plot")
              ),
              card(
                card_header("Catch vs. Weather"),
                plotOutput("weather_catch_correlation")
              )
            )
  ),
  
  nav_panel("Price Tracking",
            layout_columns(
              card(
                card_header("Price Trends"),
                plotOutput("price_trend")
              ),
              card(
                card_header("Price Analysis"),
                DTOutput("price_table")
              )
            )
  ),
  
  nav_panel("Vessel Monitoring",
            layout_columns(
              card(
                card_header("Vessel Locations"),
                leafletOutput("vessel_map", height = 400)
              ),
              card(
                card_header("Vessel Status"),
                DTOutput("vessel_status")
              )
            )
  )
)

server <- function(input, output) {
  # Filtered dataset
  filtered_data <- reactive({
    fishing_data %>%
      filter(
        location == input$location,
        species == input$species,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })
  
  # Basic outputs
  output$total_catch <- renderText({
    paste(round(sum(filtered_data()$catch_kg), 0), "kg")
  })
  
  output$daily_avg <- renderText({
    paste(round(mean(filtered_data()$catch_kg), 1), "kg")
  })
  
  output$quota_status <- renderText({
    current_catch <- sum(filtered_data()$catch_kg)
    quota <- filtered_data()$quota_limit[1]
    paste(round(current_catch/quota * 100, 1), "%")
  })
  
  # Plots
  output$trend_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = catch_kg)) +
      geom_line(color = "steelblue") +
      geom_smooth(method = "loess", se = TRUE) +
      theme_minimal() +
      labs(x = "Date", y = "Catch (kg)", title = "Daily Catch Trends")
  })
  
  # Maps
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 35, lat = -6, zoom = 5) %>%
      addCircleMarkers(
        data = filtered_data(),
        lng = ~lon, 
        lat = ~lat,
        popup = ~paste(
          "Location:", location,
          "<br>Species:", species,
          "<br>Catch:", round(catch_kg, 1), "kg"
        ),
        radius = 8,
        color = "navy",
        fillOpacity = 0.7
      )
  })
  
  # Statistical Analysis
  output$summary_stats <- renderPrint({
    data <- filtered_data()[[input$analysis_var]]
    summary_stats <- summary(data)
    cat("Summary Statistics:\n")
    print(summary_stats)
    cat("\nStandard Deviation:", sd(data), "\n")
    cat("Coefficient of Variation:", sd(data)/mean(data)*100, "%\n")
  })
  
  output$dist_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$analysis_var)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      theme_minimal() +
      labs(title = "Distribution Analysis",
           y = "Frequency")
  })
  
  # Weather integration
  output$weather_plot <- renderPlot({
    weather_data <- filtered_data() %>%
      group_by(weather_condition) %>%
      summarise(avg_catch = mean(catch_kg))
    
    ggplot(weather_data, aes(x = weather_condition, y = avg_catch)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = "Average Catch by Weather Condition",
           x = "Weather", y = "Average Catch (kg)")
  })
  
  output$weather_catch_correlation <- renderPlot({
    ggplot(filtered_data(), aes(x = weather_condition, y = catch_kg)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Catch Distribution by Weather Condition",
           x = "Weather Condition", y = "Catch (kg)")
  })
  
  # Price tracking
  output$price_trend <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = price_per_kg)) +
      geom_line(color = "darkgreen") +
      theme_minimal() +
      labs(title = "Price Trends Over Time",
           x = "Date", y = "Price per kg ($)")
  })
  
  # Vessel monitoring map
  output$vessel_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 35, lat = -6, zoom = 5) %>%
      addCircleMarkers(
        data = filtered_data(),
        lng = ~lon, 
        lat = ~lat,
        popup = ~paste(
          "Vessel:", vessel_id,
          "<br>Location:", location,
          "<br>Catch:", round(catch_kg, 1), "kg"
        ),
        radius = 8,
        color = "navy",
        fillOpacity = 0.7
      )
  })
  
  # Export capabilities
  output$download_report <- downloadHandler(
    filename = function() {
      paste("fisheries-report-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
  
  # Vessel status table
  output$vessel_status <- renderDT({
    datatable(vessel_data, options = list(pageLength = 5))
  })
  
  # Price analysis table
  output$price_table <- renderDT({
    filtered_data() %>%
      group_by(species) %>%
      summarise(
        avg_price = mean(price_per_kg),
        min_price = min(price_per_kg),
        max_price = max(price_per_kg)
      ) %>%
      datatable(options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)
