library(shiny)
library(bslib)
library(qrcode)

ui <- page_sidebar(
  theme = bs_theme(preset = "zephyr"),
  title = "QR Code Generator",
  
  sidebar = sidebar(
    title = "Settings",
    
    # Input controls
    textInput("text", "Text or URL", 
              value = "https://www.example.com"),
    
    numericInput("size", "Size (pixels)", 
                 value = 400, min = 200, max = 800, step = 50),
    
    radioButtons("ecl", "Error Correction", 
                 choices = c("Low" = "L", "Medium" = "M", 
                             "Quarter" = "Q", "High" = "H"),
                 inline = TRUE),
    
    # Action buttons
    card(
      fill = FALSE,
      class = "mt-3",
      actionButton("generate", "Generate QR Code", 
                   class = "btn-primary w-100 mb-2",
                   icon = icon("qrcode")),
      downloadButton("download", "Download PNG",
                     class = "btn-success w-100")
    )
  ),
  
  # Main panel with QR code display
  card(
    card_header(class = "bg-light", "QR Code Preview"),
    plotOutput("qr_plot")
  )
)

server <- function(input, output) {
  # Generate QR code when button is clicked
  qr_data <- eventReactive(input$generate, {
    req(input$text)
    qr_code(input$text, ecl = input$ecl)
  })
  
  # Display QR code
  output$qr_plot <- renderPlot({
    req(qr_data())
    plot(qr_data(), pointsize = input$size/50)
  })
  
  # Download handler
  output$download <- downloadHandler(
    filename = \() paste0("qrcode-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png"),
    content = function(file) {
      png(file, width = input$size, height = input$size)
      plot(qr_data(), pointsize = input$size/50)
      dev.off()
    }
  )
}

shinyApp(ui, server)
