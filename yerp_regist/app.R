library(shiny)
library(bslib)
library(qrcode)
library(grid)
library(grDevices)
library(stringr)
library(rsvg)  # Added for SVG handling

ui <- page_sidebar(
  title = "FOREST Meeting Name Tag Generator",
  theme = bs_theme(version = 5),
  
  sidebar = sidebar(
    textInput("firstname", "First Name", ""),
    textInput("lastname", "Last Name", ""),
    textInput("org", "Institution", ""),
    actionButton("generate", "Generate Name Tag", class = "btn-primary"),
    br(),
    br(),
    downloadButton("download_pdf", "Download PDF", class = "btn-success")
  ),
  
  card(
    card_header("Your Name Tag"),
    plotOutput("nametag", height = "400px"),
    uiOutput("error_message")
  )
)

server <- function(input, output, session) {
  error_state <- reactiveVal(NULL)
  
  # Load the logo image as SVG
  logo_path <- "www/BioNutra.svg"
  
  generate_nametag <- function() {
    tryCatch({
      # Set up the plot with 4:3 aspect ratio
      plot.new()
      par(bg = "white", mar = c(1, 1, 1, 1))
      plot.window(xlim = c(0, 3), ylim = c(0, 4))
      
      # Draw the border
      rect(0, 0, 3, 4, col = "white", border = "black", lwd = 2)
      
      # Add conference theme at the top
      text(1.5, 3.7, "FOREST Meeting", cex = 1.2, font = 2)
      text(1.5, 3.4, "Tanzania 2024", cex = 1, font = 3)
      
      # Add SVG logo
      if (file.exists(logo_path)) {
        tryCatch({
          # Convert SVG to raster
          logo_raster <- rsvg::rsvg_raw(logo_path)
          # Add the logo to the plot
          rasterImage(logo_raster, 
                      xleft = 2.2, ybottom = 3.3,
                      xright = 2.8, ytop = 3.9)
        }, error = function(e) {
          warning("Could not load logo: ", e$message)
        })
      }
      
      # Wrap and add name in large text
      full_name <- paste(input$firstname, input$lastname)
      wrapped_name <- str_wrap(full_name, width = 10)  # Adjust width as needed
      
      # Split wrapped name into lines and adjust position based on number of lines
      name_lines <- strsplit(wrapped_name, "\n")[[1]]
      num_lines <- length(name_lines)
      
      # Adjust starting y position based on number of lines
      start_y <- if(num_lines > 1) 2.9 else 2.7
      
      # Draw each line of the name
      for(i in 1:num_lines) {
        text(1.5, start_y - (i-1)*0.3, name_lines[i], 
             cex = 1.8, font = 2)
      }
      
      # Wrap and add institution name
      wrapped_org <- str_wrap(input$org, width = 30)  # Adjust width as needed
      org_lines <- strsplit(wrapped_org, "\n")[[1]]
      num_org_lines <- length(org_lines)
      
      # Adjust starting y position for institution based on number of lines
      org_start_y <- if(num_lines > 1) 1.9 else 2.2
      
      # Draw each line of the institution name
      for(i in 1:num_org_lines) {
        text(1.5, org_start_y - (i-1)*0.2, org_lines[i], 
             cex = 1.2, font = 3)
      }
      
      # Add QR code at the bottom
      qr_text <- paste(
        "Name:", paste(input$firstname, input$lastname),
        "\nInstitution:", input$org,
        "\nEvent: FOREST Meeting Tanzania"
      )
      qr <- qr_code(qr_text) 
      
      grid.raster(qr, x = 0.5, y = 0.3, width = 0.2)
      
      # Add a subtle bottom decoration
      rect(0, 0.1, 3, 0.2, col = "forestgreen", border = NA)
      
      error_state(NULL)
    }, error = function(e) {
      error_state(paste("Error generating name tag:", e$message))
    })
  }
  
  observeEvent(input$generate, {
    req(input$firstname, input$lastname, input$org)
    
    if (nchar(input$firstname) < 2 || nchar(input$lastname) < 2) {
      error_state("Names must be at least 2 characters long")
      return()
    }
    
    output$nametag <- renderPlot({
      generate_nametag()
    }, height = 400, width = 300) # Maintain 4:3 ratio
    
    showNotification("Name tag generated successfully!", type = "message")
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("FOREST_nametag_", input$firstname, "_", input$lastname, ".pdf")
    },
    content = function(file) {
      tryCatch({
        pdf(file, width = 3, height = 4) # Set PDF dimensions to match design
        generate_nametag()
        dev.off()
      }, error = function(e) {
        showNotification(paste("Error creating PDF:", e$message), type = "error")
      })
    }
  )
  
  output$error_message <- renderUI({
    if (!is.null(error_state())) {
      div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        error_state()
      )
    }
  })
}

shinyApp(ui, server)
