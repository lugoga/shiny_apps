require(shiny)
require(tidyverse)
require(sf)
require(vchartr)
require(bslib)


ui = page_sidebar(
  title = "Visualiztion",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    title = "Visual",
    # sliderInput("gauge", "The covered %", min = 0, max = 1, value = .8),
    
    
    numericInput("gaugei", label = "Change", value = .45)
    
    ),
  layout_columns(
    col_widths = c(4,4,4),
    card(
    ),
    card(
      card_header("Gauging", style = "font-size: 20px; font-weight: bold; font-color = green;"),
      vchartr::vchartOutput(outputId = "gauge")
    ),
    card()
    ),
  layout_columns(
    col_widths = c(6,6),
    card(),
    card()
    )
  
)


server = function(input, output, session){
  
  
  output$gauge = vchartr::renderVchart({
    req(input$gaugei)
  vchart() |> 
    v_gauge(aes(x = "Exam", y = input$gaugei))
    
  })
  
}


shinyApp(ui = ui, server = server)

