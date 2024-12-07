library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmltools)

# Define regions and areas
regions <- c(
  "Arusha", "Dar es Salaam", "Dodoma", "Geita", "Iringa", "Kagera", "Katavi",
  "Kigoma", "Kilimanjaro", "Lindi", "Manyara", "Mara", "Mbeya", "Morogoro",
  "Mtwara", "Mwanza", "Njombe", "Pemba North", "Pemba South", "Pwani",
  "Rukwa", "Ruvuma", "Shinyanga", "Simiyu", "Singida", "Songwe", "Tabora",
  "Tanga", "Zanzibar South", "Zanzibar North", "Zanzibar West"
) 
  

# Define regions and areas
regions2url <- c(
  "Arusha", "dsm", "Dodoma", "Geita", "Iringa", "Kagera", "Katavi",
  "Kigoma", "Kilimanjaro", "Lindi", "Manyara", "Mara", "Mbeya", "Morogoro",
  "Mtwara", "Mwanza", "Njombe", "Pemba North", "Pemba South", "Pwani",
  "Rukwa", "Ruvuma", "Shinyanga", "Simiyu", "Singida", "Songwe", "Tabora",
  "Tanga", "Zanzibar South", "Zanzibar North", "Zanzibar West"
) |> 
  stringr::str_to_lower()

area <- c(
  rep("Mainland", 17),      # First 17 regions are on the mainland
  rep("Zanzibar", 2),       # Pemba North and Pemba South
  rep("Mainland", 9),       # Next 9 regions on the mainland
  rep("Zanzibar", 3)        # Remaining Zanzibar regions
)

# Add coordinates for each region (approximate centers)
lat <- c(-3.3667, -6.8000, -6.1731, -2.8800, -7.7700, -1.8600, -6.3600,
         -4.8800, -3.0700, -9.9900, -3.5983, -1.7768, -8.9000, -6.8200,
         -10.2667, -2.5167, -9.3400, -5.0333, -5.3500, -7.0000, -5.6000,
         -10.6800, -3.6600, -2.6400, -4.8100, -9.0000, -5.0167, -5.0700,
         -6.1600, -5.9500, -6.1600)

lon <- c(36.6833, 39.2833, 35.7489, 32.2600, 35.7000, 31.2700, 31.1300,
         29.6300, 37.3500, 39.7100, 36.6667, 33.9833, 33.4500, 37.6700,
         40.1833, 32.9000, 34.7700, 39.7833, 39.7500, 39.3000, 32.2500,
         35.6500, 33.4200, 34.7400, 34.7500, 33.4500, 32.8000, 39.1000,
         39.2000, 39.1900, 39.2000)

# Add mock data for additional attributes
set.seed(123)  # For reproducible random data
population <- round(runif(length(regions), 500000, 5000000))
area_km2 <- round(runif(length(regions), 1000, 50000))
hospitals <- round(runif(length(regions), 5, 50))
schools <- round(runif(length(regions), 100, 1000))
rainfall_mm <- round(runif(length(regions), 500, 2000))

# Add URLs for each region
# urls <- paste0("https://lugoga.github.io/kitaa/")

region_urls = paste0("https://", regions2url,".go.tz/")

# Create photo URLs using images from www folder
photo_urls <- paste0(tolower(gsub(" ", "-", regions)), ".jpg")

# Create the data frame with all attributes including photo URLs
tanzania_regions_df <- data.frame(
  region = regions,
  area = area,
  lat = lat,
  lon = lon,
  population = population,
  area_km2 = area_km2,
  hospitals = hospitals,
  schools = schools,
  rainfall_mm = rainfall_mm,
  url = region_urls,
  photo_url = photo_urls
)

ui <- page_sidebar(
  title = "Tanzania Regions",
  sidebar = sidebar(
    selectInput("area", "Select Area", 
                choices = c("Mainland", "Zanzibar")),
    selectInput("region", "Select Region", 
                choices = NULL)
  ),
  card(
    card_header("Region Map"),
    card_body(
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  
  # Update regions based on selected area
  observe({
    
    req(input$area)
    filtered_regions = tanzania_regions_df |> 
      filter(area == input$area) |>
      pull(region)
    
    updateSelectInput(
      session = session, 
      inputId = "region",
      choices = filtered_regions
    )
  })
  
  # Create and update map
  output$map <- renderLeaflet({
    req(input$region)
    
    # Get coordinates for selected region
    # region_data <- tanzania_regions_df[tanzania_regions_df$region == input$region, ]
    
    region_data = tanzania_regions_df |> 
      filter(region == input$region)
    
    # Create custom popup content with URL and photo
    popup_content <- sprintf(
      "<div style='font-family: Arial; min-width: 300px;'>
        <h3 style='color: #2c3e50; margin-bottom: 10px; border-bottom: 2px solid #3498db;'>%s</h3>
        <img src='%s' style='width: 100%%; height: 200px; object-fit: cover; border-radius: 8px; margin-bottom: 10px;'>
        <div style='margin: 10px 0;'>
          <p style='margin: 5px 0;'><strong>📍 Area:</strong> %s</p>
          <p style='margin: 5px 0;'><strong>👥 Population:</strong> %s</p>
          <p style='margin: 5px 0;'><strong>📐 Area:</strong> %d km²</p>
          <p style='margin: 5px 0;'><strong>🏥 Hospitals:</strong> %d</p>
          <p style='margin: 5px 0;'><strong>🏫 Schools:</strong> %d</p>
          <p style='margin: 5px 0;'><strong>🌧️ Annual Rainfall:</strong> %d mm</p>
          <p style='margin: 10px 0;'>
            <strong>🔗 More Info:</strong> 
            <a href='%s' target='_blank' style='color: #3498db; text-decoration: none;'>
              Visit Official Website
              <span style='margin-left: 5px;'>↗️</span>
            </a>
          </p>
        </div>
      </div>",
      region_data$region,
      region_data$photo_url,
      region_data$area,
      format(region_data$population, big.mark=","),
      region_data$area_km2,
      region_data$hospitals,
      region_data$schools,
      region_data$rainfall_mm,
      region_data$url
    )
    
    # Create map
    leaflet() %>%
      addTiles() %>%
      setView(lng = region_data$lon, 
              lat = region_data$lat, 
              zoom = 8) %>%
      addMarkers(lng = region_data$lon, 
                 lat = region_data$lat,
                 label = region_data$region,
                 popup = popup_content
                 )
  })
}

shinyApp(ui = ui, server = server)
