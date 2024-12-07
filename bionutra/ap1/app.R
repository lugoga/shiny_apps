library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(bslib)
library(forcats)
library(plotly)

# Data preprocessing
data <- data.frame(
  City = c("Arusha", "Dar es Salaam", "Mbeya", "Dodoma", "Mwanza", "Moshi", "Morogoro", "Tanga", "Others"),
  Country = "Tanzania",
  stringsAsFactors = FALSE
)

# Create sample data with counts
diseases <- c(
  "Diabetes",
  "High Blood Pressure",
  "Obesity",
  "General Nutrition",
  "Cancer",
  "Kidney Disease",
  "Metabolic Syndrome",
  "Others"
)

# City coordinates (approximate)
city_coords <- data.frame(
  City = c("Arusha", "Dar es Salaam", "Mbeya", "Dodoma", "Mwanza", "Moshi", "Morogoro", "Tanga"),
  lat = c(-3.3667, -6.8235, -8.9000, -6.1730, -2.5167, -3.3500, -6.8219, -5.0667),
  lng = c(36.6833, 39.2695, 33.4500, 35.7419, 32.9000, 37.3333, 37.9997, 39.1000)
)

# Sample disease counts per city - fixed sampling
disease_counts <- expand.grid(City = unique(data$City), Disease = diseases)
n_rows <- nrow(disease_counts)
disease_counts$Count <- sample(1:50, n_rows, replace = TRUE)

# Custom theme
my_theme <- bs_theme(
  bootswatch = "minty",
  primary = "#2E8B57",
  "enable-shadows" = TRUE,
  "card-border-radius" = "15px",
  "font-size-base" = "0.95rem",
  "navbar-bg" = "#2E8B57"
)

ui <- page_navbar(
  title = span(
    img(src = "BioNutra logo.svg", height = "30px", style = "margin-right: 10px;"),
    "BioNutra"
  ),
  theme = my_theme,
  
  nav_panel(
    "Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        selectInput("city", "Select City:", 
                    choices = c("All", unique(data$City))),
        selectInput("disease", "Select Health Condition:",
                    choices = c("All", diseases)),
        hr(),
        "Analyze health conditions across Tanzania"
      ),
      
      # Value boxes row
      layout_column_wrap(
        width = 1/6,
        value_box(
          title = "Total Cases",
          value = textOutput("total_cases"),
          showcase = bsicons::bs_icon("clipboard-pulse"),
          theme = "primary"
        ),
        value_box(
          title = "Cities Affected",
          value = textOutput("cities_count"),
          showcase = bsicons::bs_icon("geo-alt"),
          theme = "secondary"
        ),
        value_box(
          title = "Most Common Condition",
          value = textOutput("top_disease"),
          showcase = bsicons::bs_icon("heart-pulse"),
          theme = "success"
        ),
        value_box(
          title = "Most Affected City",
          value = textOutput("top_city"),
          showcase = bsicons::bs_icon("building"),
          theme = "info"
        ),
        value_box(
          title = "Avg Cases per City",
          value = textOutput("avg_cases"),
          showcase = bsicons::bs_icon("calculator"),
          theme = "warning"
        ),
        value_box(
          title = "Top Condition %",
          value = textOutput("top_disease_percent"),
          showcase = bsicons::bs_icon("percent"),
          theme = "danger"
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        
        card(
          card_header(
            class = "bg-primary text-white",
            "Disease Distribution by City"
          ),
          plotlyOutput("barplot", height = "400px")
        ),
        
        card(
          card_header(
            class = "bg-primary text-white",
            "Geographic Distribution"
          ),
          leafletOutput("map", height = "400px")
        )
      ),
      
      layout_column_wrap(
        width = 1,
        card(
          card_header(
            class = "bg-primary text-white",
            "Top Health Conditions"
          ),
          plotlyOutput("pieChart", height = "400px")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    df <- disease_counts
    
    if (input$city != "All") {
      df <- df %>% filter(City == input$city)
    }
    
    if (input$disease != "All") {
      df <- df %>% filter(Disease == input$disease)
    }
    
    df
  })
  
  # Value box calculations
  output$total_cases <- renderText({
    sum(filtered_data()$Count)
  })
  
  output$cities_count <- renderText({
    length(unique(filtered_data()$City))
  })
  
  output$top_disease <- renderText({
    filtered_data() %>%
      group_by(Disease) %>%
      summarize(Total = sum(Count)) %>%
      arrange(desc(Total)) %>%
      slice(1) %>%
      pull(Disease)
  })
  
  output$top_city <- renderText({
    filtered_data() %>%
      group_by(City) %>%
      summarize(Total = sum(Count)) %>%
      arrange(desc(Total)) %>%
      slice(1) %>%
      pull(City)
  })
  
  output$avg_cases <- renderText({
    total <- sum(filtered_data()$Count)
    cities <- length(unique(filtered_data()$City))
    round(total / cities, 1)
  })
  
  output$top_disease_percent <- renderText({
    total <- sum(filtered_data()$Count)
    top_disease_count <- filtered_data() %>%
      group_by(Disease) %>%
      summarize(Total = sum(Count)) %>%
      arrange(desc(Total)) %>%
      slice(1) %>%
      pull(Total)
    
    paste0(round(top_disease_count / total * 100, 1), "%")
  })
  
  # Reactive values for map markers
  city_data <- reactive({
    city_summary <- filtered_data() %>%
      group_by(City) %>%
      summarize(Total = sum(Count))
    
    city_coords %>%
      left_join(city_summary, by = "City") %>%
      replace_na(list(Total = 0))
  })
  
  output$barplot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(City, -Count), y = Count, fill = Disease)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(x = "City", y = "Number of Cases") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white")
      ) +
      scale_fill_brewer(palette = "Set3")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$map <- renderLeaflet({
    data <- city_data()
    
    # Calculate radius based on total cases
    max_total <- max(data$Total)
    radius_scale <- function(x) {
      10 + (x / max_total) * 20
    }
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        popup = ~paste(
          "<strong>", City, "</strong><br>",
          "Total Cases: ", Total, "<br>",
          "<small>Click for more details</small>"
        ),
        radius = ~radius_scale(Total),
        color = "#2E8B57",
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        label = ~paste(City, ": ", Total, " cases"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        title = "Cases",
        colors = "#2E8B57",
        labels = "Number of cases",
        opacity = 0.7
      ) %>%
      setView(lng = 35.7419, lat = -6.1730, zoom = 6)
  })
  
  output$pieChart <- renderPlotly({
    disease_summary <- filtered_data() %>%
      group_by(Disease) %>%
      summarize(Total = sum(Count)) %>%
      arrange(desc(Total))
    
    plot_ly(disease_summary, labels = ~Disease, values = ~Total, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(Disease, "\nCases:", Total),
            marker = list(colors = colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(nrow(disease_summary)))) %>%
      layout(showlegend = TRUE)
  })
}

shinyApp(ui, server)
