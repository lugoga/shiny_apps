library(shiny)
library(bslib)
library(stringr)
library(bsicons)  # Added this package for icons

ui <- page_navbar(
  title = "YERP Registration System",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#2E8B57"
  ),
  
  nav_panel(
    title = "Registration",
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = "Personal Information",
        
        textInput("firstname", "First Name*"),
        textInput("lastname", "Last Name*"),
        dateInput("dob", "Date of Birth*"),
        selectInput("gender", "Gender*",
                    choices = c("", "Male", "Female", "Non-binary", "Prefer not to say")),
        textInput("email", "Email Address*"),
        textInput("phone", "Phone Number*"),
        
        selectInput("education", "Highest Education Level*",
                    choices = c("", "High School", "Bachelor's", "Master's", "PhD")),
        
        textAreaInput("institution", "Current Institution*", height = "100px"),
        
        selectInput("research_area", "Primary Research Interest*",
                    choices = c("", 
                                "Forest Ecology",
                                "Biodiversity Conservation",
                                "Climate Change",
                                "Environmental Policy",
                                "Wildlife Conservation",
                                "Sustainable Agriculture",
                                "Water Resources",
                                "Other")),
        
        textAreaInput("motivation", "Motivation for Joining YERP*",
                      height = "150px",
                      placeholder = "Please describe your motivation and what you hope to achieve through the YERP program (100-300 words)"),
        
        checkboxInput("terms", "I agree to the terms and conditions*"),
        
        actionButton("submit", "Submit Application",
                     class = "btn-primary",
                     width = "100%"),
        
        br(),
        br(),
        
        helpText("* Required fields"),
        
        status = "primary"
      ),
      
      card(
        card_header(
          "Program Overview",
          class = "bg-primary text-white"
        ),
        card_body(
          h4("Young Environmental Researchers Program (YERP)"),
          p("YERP is designed to support early-career researchers in environmental sciences, 
            focusing on forest conservation and sustainable development in Tanzania."),
          
          h5("Program Benefits:"),
          tags$ul(
            tags$li("Research funding opportunities"),
            tags$li("Mentorship from experienced scientists"),
            tags$li("Networking with international researchers"),
            tags$li("Access to advanced research facilities"),
            tags$li("Publication support")
          ),
          
          h5("Eligibility:"),
          tags$ul(
            tags$li("Age: 20-35 years"),
            tags$li("Background in environmental sciences or related fields"),
            tags$li("Commitment to environmental research"),
            tags$li("Strong academic record"),
            tags$li("Proficiency in English")
          )
        )
      ),
      
      card(
        card_header("Application Status"),
        value_box(
          title = "Status",
          value = textOutput("status"),
          showcase = bs_icon("check-circle"),  # Now this will work with bsicons package
          theme = "primary"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialize reactive values for application status
  status_value <- reactiveVal("Not Submitted")
  
  # Validation function
  validate_inputs <- function() {
    if (nchar(input$firstname) < 2) return("Please enter a valid first name")
    if (nchar(input$lastname) < 2) return("Please enter a valid last name")
    if (is.null(input$dob)) return("Please enter your date of birth")
    if (input$gender == "") return("Please select your gender")
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", input$email)) return("Please enter a valid email address")
    if (nchar(input$phone) < 10) return("Please enter a valid phone number")
    if (input$education == "") return("Please select your education level")
    if (nchar(input$institution) < 5) return("Please enter your current institution")
    if (input$research_area == "") return("Please select your research area")
    if (nchar(input$motivation) < 100) return("Please provide a longer motivation statement (minimum 100 characters)")
    if (!input$terms) return("Please accept the terms and conditions")
    return(NULL)
  }
  
  # Handle form submission
  observeEvent(input$submit, {
    # Validate inputs
    validation_result <- validate_inputs()
    
    if (!is.null(validation_result)) {
      showNotification(
        validation_result,
        type = "error"
      )
      status_value("Not Submitted")
    } else {
      # Successful submission
      showNotification(
        "Application submitted successfully! You will receive a confirmation email shortly.",
        type = "success",
        duration = 10
      )
      status_value("Submitted Successfully")
      
      # Optional: Reset form
      updateTextInput(session, "firstname", value = "")
      updateTextInput(session, "lastname", value = "")
      updateDateInput(session, "dob", value = NULL)
      updateSelectInput(session, "gender", selected = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "phone", value = "")
      updateSelectInput(session, "education", selected = "")
      updateTextAreaInput(session, "institution", value = "")
      updateSelectInput(session, "research_area", selected = "")
      updateTextAreaInput(session, "motivation", value = "")
      updateCheckboxInput(session, "terms", value = FALSE)
    }
  })
  
  # Display application status
  output$status <- renderText({
    status_value()
  })
}

shinyApp(ui, server)
