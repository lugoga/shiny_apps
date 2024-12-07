require(shiny)
require(bslib)


ui = page_sidebar(
  title = "Training buzzling",
  sidebar = sidebar(
    width = "200px", position = "left", open = "desktop", title = "LANDING",
    sliderInput(inputId = "slide", label = "sliding", min = 1, max = 10, value = 3, step = 1),
    selectInput(inputId = "zone", label = "Choose Zones", choices = c("Northern", "Eastern", "Southern", "Lake")),
    radioButtons(inputId = "crop", label = "Choose crop", choices = c("Rice", "Maize", "Beans")),
    dateInput(inputId = "date", label = "Choose season", min = "2024-01-01", max = "2024-12-30", value = c("2024-03-01", "2024-06-30")),
    
  ),
  "main contents",
  plotOutput("map")
  
)


server = function(input, output, session) {
  
}


shinyApp(ui = ui, server = server)

# runApp("02_ui")
