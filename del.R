require(shiny)
require(qrcode)

ui = page_sidebar(
  title = "Chapuo", 
  sidebar = sidebar(
    title = "MENU",
    dateInput("date","Choose date", value = "2024-12-05"),
    textInput("firstname", "First Name"),
    textInput("surname", "Surname"),
    radioButtons("res", "Choose Residence", choices = c("Residence", "Non-Residence"), inline = FALSE),
    helpText("Please Choose a region where you reside in Tanzania at the moment"),
    textAreaInput(inputId = 'greeting', NULL, height = "80px")
    ),
  layout_columns(
    col_widths = c(3,3,3,3),
    card(
      layout_columns(
        col_widths = c(6,6),
        card(),
        card()
        ),
    ),
    card(),
    card(),
    card(plotOutput("mark"))
    )
)

server = function(input, output, session){
  
  needed = reactive({
    req(input$date)
    req(input$firstname)
    req(input$surname)
    req(input$res)
    req(input$greeting)
    paste(input$firstname, input$surname, input$season, input$res, input$greeting)
  })
  

  output$mark = renderPlot({
    
    qr_code(needed()) |> 
      plot()
  })
  
}

shinyApp(ui = ui, server = server)

