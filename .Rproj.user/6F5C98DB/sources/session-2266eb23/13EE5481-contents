library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(DT)

# Create/connect to DuckDB database
con <- dbConnect(duckdb(), dbdir = "records.duckdb", read_only = FALSE)

# Create a table if it doesn't exist
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS records (
    id INTEGER PRIMARY KEY,
    name VARCHAR(100),
    email VARCHAR(100),
    age INTEGER,
    comments TEXT
  )
")

# Create a sequence for id if it doesn't exist
dbExecute(con, "
  CREATE SEQUENCE IF NOT EXISTS records_id_seq;
")

ui <- page_sidebar(
  title = "Record Entry System",
  sidebar = sidebar(
    title = "Enter Record Details",
    
    # Form inputs
    textInput("name", "Full Name", ""),
    textInput("email", "Email Address", ""),
    numericInput("age", "Age", value = 18, min = 0, max = 120),
    textAreaInput("comments", "Comments", "", height = "100px"),
    
    actionButton("submit", "Submit Record", class = "btn-primary"),
    hr(),
    actionButton("view", "View Records", class = "btn-secondary")
  ),
  
  # Main panel with card to show feedback and data
  card(
    card_header("Database Records"),
    uiOutput("feedback"),
    DTOutput("records_table")
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store feedback message
  feedback <- reactiveVal("")
  
  # Handle form submission
  observeEvent(input$submit, {
    tryCatch({
      # Validate inputs
      if (input$name == "" || input$email == "") {
        feedback(paste0("Error: Name and email are required fields"))
        return()
      }
      
      # Insert record into database using nextval for id
      dbExecute(con,
                "INSERT INTO records (id, name, email, age, comments) 
         VALUES (nextval('records_id_seq'), ?, ?, ?, ?)",
                list(input$name, input$email, input$age, input$comments)
      )
      
      # Clear form
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "email", value = "")
      updateNumericInput(session, "age", value = 18)
      updateTextAreaInput(session, "comments", value = "")
      
      feedback("Record added successfully!")
      
    }, error = function(e) {
      feedback(paste0("Error: ", e$message))
    })
  })
  
  # Display feedback message
  output$feedback <- renderUI({
    if (feedback() != "") {
      if (grepl("^Error:", feedback())) {
        div(class = "alert alert-danger", feedback())
      } else {
        div(class = "alert alert-success", feedback())
      }
    }
  })
  
  # View records when button is clicked
  observeEvent(input$view, {
    output$records_table <- renderDT({
      dbGetQuery(con, "SELECT * FROM records ORDER BY id")
    }, options = list(pageLength = 5))
  })
  
  # Clean up database connection when session ends
  onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)
