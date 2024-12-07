library(shiny)
library(bslib)
library(dplyr)
library(tibble)

# # Create tibble with health conditions and their biomarkers
# health_data <- tibble(
#   condition = c(
#     "Hypertension", "Hypertension",
#     "Metabolic Syndrome", "Metabolic Syndrome", "Metabolic Syndrome",
#     "Heart Disease",
#     "Fatty Liver Disease", "Fatty Liver Disease",
#     "Glucose Intolerance", "Glucose Intolerance", "Glucose Intolerance",
#     "Type 2 Diabetes", "Type 2 Diabetes", "Type 2 Diabetes",
#     "Cardiovascular Health", "Cardiovascular Health",
#     "Anemia", "Anemia",
#     "Weight Status"
#   ),
#   biomarker = c(
#     "Systolic", "Diastolic",
#     "Waist circumference (Males)", "Waist circumference (Females)", "TG: HDL Ratio",
#     "Serum Homocysteine",
#     "ALT (Caucasians)", "ALT (African Americans)",
#     "Fasting Glucose", "2-Hour Glucose", "HbA1c",
#     "Fasting Glucose", "2-Hour Glucose", "HbA1c",
#     "HDL-C (Men)", "HDL-C (Women)",
#     "Hemoglobin (Men)", "Hemoglobin (Women)",
#     "BMI"
#   ),
#   optimal_value = c(
#     120, 80,
#     40, 35, 2.5,
#     15,
#     25, 20,
#     5.6, 7.8, 6.0,
#     7.0, 11.1, 6.5,
#     40, 50,
#     13.5, 12.0,
#     25
#   ),
#   units = c(
#     "mmHg", "mmHg",
#     "inches", "inches", "ratio",
#     "μmol/L",
#     "IU/L", "IU/L",
#     "mmol/L", "mmol/L", "%",
#     "mmol/L", "mmol/L", "%",
#     "mg/dL", "mg/dL",
#     "g/dL", "g/dL",
#     "kg/m²"
#   ),
#   comparison_type = c(
#     "less", "less",
#     "less", "less", "less",
#     "less",
#     "less", "less",
#     "less", "less", "less",
#     "less", "less", "less",
#     "greater", "greater",
#     "greater", "greater",
#     "less"
#   ),
#   min_value = c(
#     80, 40,  # Hypertension
#     20, 20, 0,  # Metabolic Syndrome
#     5,  # Heart Disease
#     10, 10,  # Fatty Liver Disease
#     3, 4, 4,  # Glucose Intolerance
#     4, 6, 4,  # Type 2 Diabetes
#     20, 30,  # Cardiovascular Health
#     8, 8,  # Anemia
#     15  # BMI
#   ),
#   max_value = c(
#     200, 120,  # Hypertension
#     60, 55, 5,  # Metabolic Syndrome
#     30,  # Heart Disease
#     50, 50,  # Fatty Liver Disease
#     10, 15, 10,  # Glucose Intolerance
#     15, 20, 10,  # Type 2 Diabetes
#     80, 90,  # Cardiovascular Health
#     20, 18,  # Anemia
#     45  # BMI
#   )
# ) %>%
#   mutate(
#     condition = factor(condition),
#     comparison_type = factor(comparison_type)
#   )


health_data = read_csv("health_biomarkers.csv")

ui <- page(
  title = "Health Indicator Dashboard",
  theme = bs_theme(bootswatch = "minty"),
  
  card(
    card_header("Health Status Assessment"),
    
    layout_column_wrap(
      width = "250px",
      fillable = TRUE,
      
      # Input controls card
      card(
        card_header("Input Parameters"),
        selectInput("condition", "Health Condition/Disease",
                    unique(health_data$condition)),
        
        selectInput("biomarker", "Biomarker/Indicator",
                    NULL),
        
        uiOutput("value_slider"),
        
        textOutput("unit_display")
      ),
      
      # Status box
      uiOutput("status_box"),
      
      # Optimal value box
      value_box(
        title = "Optimal Value",
        value = textOutput("optimal"),
        showcase = bsicons::bs_icon("check-circle"),
        full_screen = TRUE
      )
    ),
    
    card_body(
      textOutput("interpretation")
    )
  )
)

server <- function(input, output, session) {
  
  # Update biomarker choices based on selected condition
  observe({
    biomarker_choices <- health_data %>%
      filter(condition == input$condition) %>%
      pull(biomarker)
    
    updateSelectInput(session, "biomarker",
                      choices = biomarker_choices)
  })
  
  # Dynamic slider UI
  output$value_slider <- renderUI({
    req(input$biomarker)
    
    biomarker_data <- health_data %>%
      filter(biomarker == input$biomarker)
    
    sliderInput(
      "value",
      "Enter Value",
      min = biomarker_data$min_value,
      max = biomarker_data$max_value,
      value = biomarker_data$optimal_value,
      step = if(grepl("%|ratio", biomarker_data$units)) 0.1 else 1
    )
  })
  
  # Get current biomarker data
  current_data <- reactive({
    health_data %>%
      filter(condition == input$condition,
             biomarker == input$biomarker)
  })
  
  # Display unit
  output$unit_display <- renderText({
    req(current_data())
    paste("Unit:", current_data()$units)
  })
  
  # Determine status and color
  status_info <- reactive({
    req(current_data(), input$value)
    
    optimal <- current_data()$optimal_value
    comparison <- current_data()$comparison_type
    
    if (comparison == "less") {
      if (input$value <= optimal) {
        list(status = "OPTIMAL", color = "success")
      } else {
        list(status = "ABOVE OPTIMAL", color = "danger")
      }
    } else {
      if (input$value >= optimal) {
        list(status = "OPTIMAL", color = "success")
      } else {
        list(status = "BELOW OPTIMAL", color = "info")
      }
    }
  })
  
  # Dynamic status box with color
  output$status_box <- renderUI({
    req(status_info())
    status_data <- status_info()
    
    value_box(
      title = "Status",
      value = status_data$status,
      showcase = bsicons::bs_icon("heart-pulse"),
      theme = status_data$color,
      full_screen = TRUE
    )
  })
  
  # Display optimal value
  output$optimal <- renderText({
    req(current_data())
    paste(current_data()$optimal_value, current_data()$units)
  })
  
  # Provide interpretation
  output$interpretation <- renderText({
    req(current_data())
    
    optimal <- current_data()$optimal_value
    comparison <- current_data()$comparison_type
    units <- current_data()$units
    
    if (comparison == "less") {
      if (input$value <= optimal) {
        paste("Your value of", input$value, units, "is within the optimal range (≤", optimal, units, ").")
      } else {
        paste("Your value of", input$value, units, "is above the optimal range (≤", optimal, units, "). Consider consulting a healthcare provider.")
      }
    } else {
      if (input$value >= optimal) {
        paste("Your value of", input$value, units, "is within the optimal range (≥", optimal, units, ").")
      } else {
        paste("Your value of", input$value, units, "is below the optimal range (≥", optimal, units, "). Consider consulting a healthcare provider.")
      }
    }
  })
}

shinyApp(ui, server)
