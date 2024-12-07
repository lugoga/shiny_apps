library(shiny)
library(bslib)
library(dplyr)

# Helper functions remain the same
evaluate_health <- function(value, condition, gender = NULL, ethnicity = NULL) {
  status <- switch(condition,
                   "systolic" = if(value < 120) "Normal" else if(value < 140) "Elevated" else "High",
                   "diastolic" = if(value < 80) "Normal" else if(value < 90) "Elevated" else "High",
                   "waist" = {
                     if(gender == "Male") {
                       if(value < 94) "Normal" else "Elevated"
                     } else {
                       if(value < 80) "Normal" else "Elevated"
                     }
                   },
                   "fasting_glucose" = if(value < 100) "Normal" else if(value < 126) "Prediabetes" else "Diabetes Risk",
                   "hba1c" = if(value < 5.7) "Normal" else if(value < 6.5) "Prediabetes" else "Diabetes Risk"
  )
  return(status)
}

get_advice <- function(condition, status) {
  switch(status,
         "Normal" = "Continue maintaining healthy lifestyle habits",
         "Elevated" = "Consider lifestyle modifications and consult healthcare provider",
         "High" = "Consult healthcare provider for evaluation",
         "Prediabetes" = "Consult healthcare provider for diabetes prevention plan",
         "Diabetes Risk" = "Urgent: Consult healthcare provider for diabetes management"
  )
}

ui <- page(
  theme = bs_theme(
    version = 5,
    bootswatch = "lumen",
    primary = "#2C3E50",
    "navbar-bg" = "#2C3E50"
  ),
  
  title = "Health Status Assessment",
  
  layout_columns(
    col_widths = c(4, 8),
    gap = "1rem",
    
    # Left column - Inputs
    card(
      card_header("Patient Information", class = "bg-primary text-white"),
      
      selectInput("gender", "Gender", c("Male", "Female")),
      numericInput("age", "Age", value = 30, min = 18, max = 100),
      
      card_header("Key Measurements", class = "bg-primary text-white mt-3"),
      
      layout_columns(
        col_widths = c(6, 6),
        numericInput("weight", "Weight (kg)", value = 70),
        numericInput("height", "Height (m)", value = 1.70)
      ),
      layout_columns(
        col_widths = c(6, 6),
        numericInput("systolic", "Systolic", value = 120),
        numericInput("diastolic", "Diastolic", value = 80)
      ),
      numericInput("waist", "Waist (cm)", value = 90),
      numericInput("fasting_glucose", "Fasting Glucose", value = 95),
      numericInput("hba1c", "HbA1c (%)", value = 5.5)
    ),
    
    # Right column - Results
    card(
      card_header("Health Overview", class = "bg-primary text-white"),
      
      # Status boxes
      layout_columns(
        value_box(
          "Cardiovascular",
          uiOutput("bp_status"),
          showcase = bsicons::bs_icon("heart-pulse"),
          class = "bg-gradient"
        ),
        value_box(
          "Metabolic",
          uiOutput("metabolic_status"),
          showcase = bsicons::bs_icon("activity"),
          class = "bg-gradient"
        ),
        value_box(
          "Diabetes",
          uiOutput("diabetes_status"),
          showcase = bsicons::bs_icon("graph-up"),
          class = "bg-gradient"
        )
      ),
      
      # Detailed cards
      layout_columns(
        col_widths = c(6, 6),
        gap = "1rem",
        
        card(
          card_header("Risk Assessment", class = "bg-secondary text-white"),
          uiOutput("risk_indicators")
        ),
        
        card(
          card_header("Key Actions", class = "bg-secondary text-white"),
          uiOutput("key_actions")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Calculate BMI
  bmi <- reactive({
    input$weight / (input$height ^ 2)
  })
  
  # Health status calculation
  health_status <- reactive({
    list(
      bp = list(
        systolic = input$systolic,
        diastolic = input$diastolic,
        status = evaluate_health(input$systolic, "systolic")
      ),
      metabolic = list(
        bmi = round(bmi(), 1),
        waist = input$waist,
        status = evaluate_health(input$waist, "waist", input$gender)
      ),
      diabetes = list(
        glucose = input$fasting_glucose,
        hba1c = input$hba1c,
        status = evaluate_health(input$fasting_glucose, "fasting_glucose")
      )
    )
  })
  
  # Status Outputs with Icons
  output$bp_status <- renderUI({
    status <- health_status()$bp$status
    color <- if(status == "Normal") "success" else if(status == "High") "danger" else "warning"
    div(
      class = paste0("text-", color),
      h3(status, class = "mb-0"),
      p(paste(health_status()$bp$systolic, "/", health_status()$bp$diastolic), 
        class = "small text-muted mb-0")
    )
  })
  
  output$metabolic_status <- renderUI({
    status <- health_status()$metabolic$status
    color <- if(status == "Normal") "success" else "warning"
    div(
      class = paste0("text-", color),
      h3(status, class = "mb-0"),
      p(paste("BMI:", health_status()$metabolic$bmi), 
        class = "small text-muted mb-0")
    )
  })
  
  output$diabetes_status <- renderUI({
    status <- health_status()$diabetes$status
    color <- if(status == "Normal") "success" else if(status == "Diabetes Risk") "danger" else "warning"
    div(
      class = paste0("text-", color),
      h3(status, class = "mb-0"),
      p(paste("HbA1c:", health_status()$diabetes$hba1c, "%"), 
        class = "small text-muted mb-0")
    )
  })
  
  # Risk Indicators
  output$risk_indicators <- renderUI({
    status <- health_status()
    
    div(
      class = "p-3",
      lapply(names(status), function(category) {
        status_color <- switch(status[[category]]$status,
                               "Normal" = "success",
                               "High" = "danger",
                               "warning"
        )
        
        div(
          class = "mb-3",
          h5(tools::toTitleCase(category)),
          div(
            class = paste0("alert alert-", status_color, " py-2 mb-0"),
            status[[category]]$status
          )
        )
      })
    )
  })
  
  # Key Actions
  output$key_actions <- renderUI({
    status <- health_status()
    
    recommendations <- lapply(names(status), function(category) {
      div(
        class = "mb-3",
        h6(tools::toTitleCase(category)),
        p(get_advice(category, status[[category]]$status),
          class = "small text-muted")
      )
    })
    
    div(class = "p-3", recommendations)
  })
}

shinyApp(ui, server)
