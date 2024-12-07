# packages ----
library(tidyverse)
library(leaflet)
require(shiny)
library(shinyWidgets)
library(bslib)
require(bsicons)
library(forcats)
library(plotly)
library(DT)
library(scales)
require(treemapify)

library(RSQLite)
library(DBI)

# data -----
## healthy data----

health_data = read_csv("health_biomarkers.csv")

# Data preprocessing ----
cities <- c("Arusha", "Dar es Salaam", "Mbeya", "Dodoma", "Mwanza", "Moshi", "Morogoro", "Tanga", "Others")
diseases <- c("Diabetes", "High Blood Pressure", "Obesity", "General Nutrition", 
              "Cancer", "Kidney Disease", "Metabolic", "Others")

# Define Tanzania regions
tanzania_regions <- c(
  "Select" = "",
  "Arusha",
  "Dar es Salaam",
  "Dodoma",
  "Geita",
  "Iringa",
  "Kagera",
  "Katavi",
  "Kigoma",
  "Kilimanjaro",
  "Lindi",
  "Manyara",
  "Mara",
  "Mbeya",
  "Morogoro",
  "Mtwara",
  "Mwanza",
  "Njombe",
  "Pemba North",
  "Pemba South",
  "Pwani",
  "Rukwa",
  "Ruvuma",
  "Shinyanga",
  "Simiyu",
  "Singida",
  "Songwe",
  "Tabora",
  "Tanga",
  "Zanzibar Central/South",
  "Zanzibar North",
  "Zanzibar Urban/West"
)


# Database connection setup
db <- dbConnect(SQLite(), "yerp_members.db")

# Create table if it doesn't exist
dbExecute(db, "
  CREATE TABLE IF NOT EXISTS yerp_members (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT,
    first_name TEXT,
    last_name TEXT,
    gender TEXT,
    date_of_birth TEXT,
    email TEXT,
    phone TEXT,
    region TEXT,
    country TEXT,
    height TEXT,
    weight TEXT,
    health_conditions TEXT,
    diet_plan TEXT,
    payment_method TEXT
  )
")


# City coordinates
city_coords <- data.frame(
  City = cities[1:8],  # Excluding "Others"
  lat = c(-3.3667, -6.8235, -8.9000, -6.1730, -2.5167, -3.3500, -6.8219, -5.0667),
  lng = c(36.6833, 39.2695, 33.4500, 35.7419, 32.9000, 37.3333, 37.9997, 39.1000)
)

# Generate sample data
set.seed(123)
disease_counts <- expand.grid(
  City = cities,
  Disease = diseases,
  stringsAsFactors = FALSE
) %>%
  mutate(Count = sample(1:50, n(), replace = TRUE))


# my_theme = bs_theme(
#   bootswatch = "minty",
#   primary = "#00480F", 
#   # secondary = "#C6E3CC",
#   "enable-shadows" = TRUE,
#   "card-border-radius" = "15px" 
# )

my_theme <- bs_theme(
  version = 5,
  primary = "#00480F",
  # Adjusting a few other properties to complement the primary color
  "enable-shadows" = TRUE,
  "body-bg" = "#C6E3CC",
  # bg = "black",
  # fg = "steelblue",
  "border-radius" = "0.5rem",
  "card-border-radius" = "15px",
  base_font = font_google("Poppins")
) 


# UI ----
ui <- page_navbar(
  fillable = FALSE,
  title = span(
    img(src = "BioNutra logo -01_without_name.svg", height = "40px", style = "margin-right: 10px;"),
    "BioNutra"
  ),
  theme = my_theme,
  
  # Add Google Fonts link
  tags$head(
    
    # # Font Awesome
    # tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    # 
    # Font Poppins
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap" ),
    tags$style("
      * {
        font-family: 'Poppins', sans-serif !important;
      }
      .value-box {
        font-family: 'Poppins', sans-serif !important;
      }
      .card-header {
        font-family: 'Poppins', sans-serif !important;
        font-weight: 600;
      }
      .sidebar {
        font-family: 'Poppins', sans-serif !important;
      }
    "),
    
    # Font Awesome
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style("
      .modal-content {
        border-radius: 15px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.3);
      }
      .modal-header {
        background-color: #00480F;
        color: white;
        border-radius: 15px 15px 0 0;
      }
      .modal-body {
        padding: 25px;
      }
      .benefit-icon {
        font-size: 2.5em;
        color: #00480F;
        margin-bottom: 20px;
      }
      .modal-body ul {
        list-style: none;
        padding-left: 0;
      }
      .modal-body li {
        margin: 10px 0;
        padding-left: 30px;
        position: relative;
      }
      .modal-body li:before {
        content: '\\f00c';
        font-family: 'Font Awesome 5 Free';
        font-weight: 900;
        position: absolute;
        left: 0;
        color: #00480F;
      }
    "),
    
    
    tags$style(HTML("
      .custom-popup .modal-content {
        background-color: #C6E3CC;
      }
      .message-text {
        font-size: 48px;
        color: #00480F;
        font-weight: bold;
        padding: 20px;
        text-align: center;
        line-height: 1.3;
      }
      .modal-title {
        color: #00480F;
        font-size: 24px;
        font-weight: bold;
      }
      .btn-custom {
        background-color: #00480F;
        color: white;
        font-size: 20px;
        padding: 15px 30px;
        border-radius: 25px;
        border: none;
        transition: all 0.3s ease;
      }
      .btn-custom:hover {
        background-color: #00480F;
        color: white;
        transform: scale(1.05);
        box-shadow: 0 4px 15px rgba(0,130,27,0.3);
      }
      .btn-close-custom {
        background-color: #00480F;
        color: white;
        font-size: 18px;
        padding: 10px 25px;
        border-radius: 20px;
        border: none;
        transition: all 0.3s ease;
        margin-top: 20px;
      }
      .btn-close-custom:hover {
        background-color: #006115;
        color: white;
        transform: scale(1.05);
        box-shadow: 0 4px 15px rgba(0,130,27,0.3);
      }
    "))
  ),
  
 
  # YERP ----
  nav_panel(
    title = "YERP",
    div(
      class = "vertical-center",
      div(
        class = "container",
        div(
          class = "row justify-content-center",
          div(
            class = "col-md-10 text-center",
            h1("YEAR-LONG EATING RIGHT PROGRAM (YERP)", class = "main-title", style = "font-size: 48px; font-weight: bold; color: #00480F;" ),
            h4("Stay Healthy and Active Year-round with Our Electrical Foods",   class = "main-title", style = "color: #00480F;"),
          )
        ),
        
        div(
          style = "text-align: center; padding: 20px;",
          actionButton(
            "show_popup", 
            label = span(
              bs_icon("bell-fill"), 
              "Click for a Surprise!"
            ),
            class = "btn-custom"
          )
        ),
        
        tags$br(),
        tags$br(),
        
        
        layout_columns(
          col_widths = c(4,4,4),
          card(
            card_header(tags$span("The Health Crisis", style = "font-size: 24px; color: #00480F; font-weight: bold;")),
            card_body("Poor diets in modern society contribute to chronic illnesses, low energy, and weakened immunity, impacting productivity and quality of life, highlighting the need for effective solutions"),
            img(src = "junk.svg", height = "200px", style = "margin-right: 10px;")
            
          ),
          card(
            card_header(tags$span("The YERP Solution", style = "font-size: 24px; color: #00480F; font-weight: bold;")),
            card_body("BioNutra’s Year-Long Eating Right Program (YERP) offers a science-backed proactive approach with electrical foods that address the root causes of dietary health issues."),
            img(src = "yerp_soln.png", height = "200px", style = "margin-right: 10px;")
          ),
          card(
            card_header(tags$span("Community Involvement", style = "font-size: 24px; color: #00480F; font-weight: bold;")),
            card_body(" BioNutra partners with communities to incorporate electrical foods into daily diets, promoting sustainable habits that address the root causes of metabolic diseases."),
            img(src = "community.svg", height = "200px", style = "margin-right: 10px;")
            )
        ),
        br(),
        br(),
        layout_column_wrap(
          
          div(
            class = "row justify-content-center",
            div(
              class = "col-md-10 text-start",
              h3(str_to_upper("A Science-backed Personalized Diet Program"), class = "main-title", style = "color: #00480F; font-size: 36px; font-weight: 500;")
              ),
            div(
              class = "col-md-10 text-start",
              helpText("A science-backed personalized diet program combines advanced nutritional research with tailored meal plans to support individual health goals. By focusing on nutrient-rich, natural foods, this program promotes healing, energy, and disease prevention. It adapts to personal needs, ensuring sustainable habits that improve overall well-being and empower healthier, long-term lifestyle choices."),
              )
          )
          
        ),
        div(
          class = "wellbeing-container",
          # First row
          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",
            value_box(
              title = span("Program Duration", style = "color: #00480F;"),
              value = span("12 Months", style = "color: #00480F;"),
              showcase = bsicons::bs_icon("calendar-check-fill", size = "3em", style = "color: #00480F;"),
              p("Transform your health with our year-round support system", style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            value_box(
              title = span("Personalized Plans", style = "color: #00480F;"),
              value = span("Diet Plans", style = "color: #00480F;"),
              showcase = bsicons::bs_icon("journal-medical", size = "3em", style = "color: #20B2AA;"),
              p("Quarterly customized nutrition plans tailored to your needs",  style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            )
            ,
            value_box(
              title = span("Program Success Rate", style = "color: #00480F;"),
              value = span("90%", style = "color: #00480F;"),
              showcase = bsicons::bs_icon("graph-up-arrow", size = "3em", style = "color: #3CB371;"),
              p("Participants achieving their desired health goals",  style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            value_box(
              title = span("Expert Support Access", style = "color: #00480F;"),
              value = span("24/7", style = "color: #00480F;"),
              showcase = bsicons::bs_icon("people-fill", size = "3em", style = "color: #228B22;"),
              p("Round-the-clock access to nutrition professionals", style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            )
          ),
          # second row
          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",
            value_box(
              title = span("Nutrition Workshops", style = "color: #00480F;"),
              value = span("12 Sessions", style = "color: #00480F;"),
              showcase = bsicons::bs_icon("easel2-fill", size = "3em", style = "color: #006400;"),
              p("Monthly interactive workshops on healthy eating and lifestyle", style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            value_box(
              title = span("Recipe Database", style = "color: #00480F;"),
              value = span("200+", style = "color: #00480F;"),
              showcase = bsicons::bs_icon("journal-bookmark-fill", size = "3em", style = "color: #32CD32;"),
              p("Access to exclusive healthy recipes and meal ideas",  style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            
            value_box(
              title = span("Health Tracking", style = "color: #00480F;"),
              value = span("Weekly", style = "color: #00480F;"),
              showcase = bsicons::bs_icon("clipboard2-pulse-fill", size = "3em", style = "color: #008B8B;"),
              p("Regular monitoring of your health metrics and progress", style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            value_box(
              title = span("Community Support", style = "color: #00480F;"),
              value = span("1000+", style = "color: #00480F;"),
              showcase = bsicons::bs_icon("geo", size = "5rem", style = "color: #4CAF50;"),
              p("Join our growing community of health-conscious individuals", style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            )
          )
        ),
        br(),
        br(),
        layout_column_wrap(
          
          div(
            class = "row justify-content-center",
            div(
              class = "col-md-10 text-start",
              h3(str_to_upper("The Articulated Program and Billing Plans"), class = "main-title", style = "font-size: 36px; font-weight: 500; color: #00480F;"),
              div(
                class = "col-md-10 text-start",
              helpText("The Year-Long Eating Right Program (YERP) is thoughtfully designed to provide a personalized, sustainable path to wellness. With flexible and affordable billing plans, clients can choose annual, semi-annual, or quarterly payment options. This ensures accessibility for everyone, empowering individuals to embrace healthier lifestyles without financial barriers or rigid commitments."),
            )
          ),
          
        ))
        ,
        
        layout_columns(
          col_widths = c(4,8),
          card(
            max_height = "100%",
            card_header(tags$span("YERP Program Benefits", style = "font-size: 24px; color: #00480F; font-weight: bold;")),
            helpText("Click any button to learn more about nutrition and wellness benefits that YERP program provides!"),
            card(
              height = "250px",
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 20px; align-items: center;",
                  
                  actionButton("mealplans", HTML(paste(bs_icon("basket"), "Personalized Diet Plans")), 
                               class = "btn-lg btn-primary", 
                               style = "width: 280px; font-size: 18px; font-weight: normal;"),
                  
                  actionButton("duration", HTML(paste(bs_icon("calendar-week"), "Flexible Meal Plans")), 
                               class = "btn-lg btn-primary", 
                               style = "width: 280px; font-size: 18px; font-weight: normal;"),
                  
                  actionButton("coaching", HTML(paste(bs_icon("person-video3"), "Nutrition Coaching")), 
                               class = "btn-lg btn-primary", 
                               style = "width: 280px; font-size: 18px; font-weight: normal;"),
                  
                  actionButton("brain", HTML(paste(bs_icon("lightning-charge"), "Enhance Brain")), 
                               class = "btn-lg btn-primary", 
                               style = "width: 280px; font-size: 18px; font-weight: normal;"),
                  
                  actionButton("gut", HTML(paste(bs_icon("heart-pulse"), "Support immune system")), 
                               class = "btn-lg btn-primary", 
                               style = "width: 280px; font-size: 18px; font-weight: normal;"),
                  
                  actionButton("liver", HTML(paste(bs_icon("shield-plus"), "Detoxify the body")), 
                               class = "btn-lg btn-primary", 
                               style = "width: 280px; font-size: 18px; font-weight: normal;"),
                  
                  actionButton("insulin", HTML(paste(bs_icon("shield-plus"), "Improve insulin")), 
                               class = "btn-lg btn-primary", 
                               style = "width: 280px; font-size: 18px; font-weight: normal;"),
                  
                )
              )
            )
          ),
          
          card(
            max_height = "100%",
            card_header(tags$span("YERP Billing Plans", style = "font-size: 24px; color: #00480F; font-weight: bold;")),
            div(
              class = "table-responsive p-3",
              tags$div(class = "pricing-cards",
                       layout_column_wrap(
                         width = 1/3,
                         card(
                           card_header(
                             div(
                               class = "d-flex align-items-center",
                               bsicons::bs_icon(name = "cash-coin", size = "2.5em", style = "color: #00480F; margin-right: 12px;"),
                               tags$span("Annual", style = "font-size: 24px; color: #00480F; font-weight: 500;")
                             )
                           ),
                           tags$div(
                             class = "text-center p-4",
                             h2("1,000,000", 
                                style = "color: #00480F; font-weight: 600; margin-bottom: 20px; font-size: 32px;"),
                             div(
                               class = "mb-3",
                               tags$span("250,000", style = "font-size: 20px; color: #444; font-weight: 500;"),
                               tags$span(" per quarter", style = "font-size: 16px; color: #666;")
                             ),
                             p("(Paid Upfront)", 
                               style = "color: #666; font-size: 14px; font-style: italic;")
                           )
                         ),
                         
                         card(
                           card_header(
                             div(
                               class = "d-flex align-items-center",
                               bsicons::bs_icon(name = "wallet", size = "2.5em", style = "color: #00480F; margin-right: 12px;"),
                               tags$span("Semi-Annual", style = "font-size: 24px; color: #00480F; font-weight: 500;")
                             )
                           ),
                           tags$div(
                             class = "text-center p-4",
                             h2("1,199,500 ", 
                                style = "color: #00480F; font-weight: 600; margin-bottom: 20px; font-size: 32px;"),
                             div(
                               class = "mb-3",
                               tags$span("299,625", style = "font-size: 20px; color: #444; font-weight: 500;"),
                               tags$span(" per quarter", style = "font-size: 16px; color: #666;")
                             ),
                             p("(Two Payments)", 
                               style = "color: #666; font-size: 14px; font-style: italic;")
                           )
                         ),
                        
                         
                      
                         
                         card(
                           card_header(
                             div(
                               class = "d-flex align-items-center",
                               bsicons::bs_icon(name = "wallet2", size = "2.5em", style = "color: #00480F; margin-right: 12px;"),
                               tags$span("Quarterly", style = "font-size: 24px; color: #00480F; font-weight: 500;")
                             )
                           ),
                           tags$div(
                             class = "text-center p-4",
                             h2("1,410,000", 
                                style = "color: #00480F; font-weight: 600; margin-bottom: 20px; font-size: 32px;"),
                             div(
                               class = "mb-3",
                               tags$span("352,500", style = "font-size: 20px; color: #444; font-weight: 500;"),
                               tags$span(" per quarter", style = "font-size: 16px; color: #666;")
                             ),
                             p("(Four Payments)", 
                               style = "color: #666; font-size: 14px; font-style: italic;")
                             )
                           )
                         )
                       )
              )
          )
        ),
        br(),
        br(),
        layout_column_wrap(
          
          div(
            class = "row justify-content-center",
            div(
              class = "col-md-10 text-start",
              h3("Join YERP: Your Path to Year-Round Wellness!!!", class = "main-title", style = "font-size: 36px; font-weight: 500; color: #00480F;"),
              div(
                class = "col-md-10 text-start",
                helpText("Joining the Year-Long Eating Right Program (YERP) is easy and straightforward. Simply fill out the form, and we will contact you to guide you through the personalized plan. Start your wellness journey today!"),
              )
            ),
            
          )
          ),
        
        layout_columns(
          card(
            # First row of inputs
            textInput("first_name", "First Name*"),
            textInput("last_name", "Last Name*"),
            dateInput("date_of_birth", "Date of Birth*", min = "1950-01-01", max = Sys.Date(), value = "1970-06-18" ),
            textInput("email", "Email Address*"),
            textInput("phone", "Phone Number*"),
           
          ),
          card(
            # Second row of inputs
            radioButtons("gender", "Gender*",  choices = c("Male", "Female"), selected = "Female"),
            selectInput("region", "Select Region*", choices = tanzania_regions),
            selectInput(inputId = "country", label = "Select Country*", 
                        choices = c("Select" = "", sort(countrycode::codelist$country.name.en))),
            numericInput("height", "Height (cm)*", value = NA),
            numericInput("weight", "Weight (kg)*", value = NA)
           
          ),
          card(
            # Third row of inputs
            selectInput("health_conditions", "Health Conditions*",
                        multiple = TRUE,
                        choices = c("Select" = "", "Diabetes", "High Blood Pressure", "Obesity", "General Nutrition", 
                                    "Cancer", "Kidney Disease", "Metabolic", "Others")),
            radioButtons("diet_plan", "Diet Plan*",
                        choices = c(
                          "Annual",
                          "Semi-Annual",
                          "Quarterly"), selected = "Annual"),
            radioButtons("payment_method", "Payment Method*",
                        choices = c("Select" = "", 
                                    "Mobile",
                                    "Credit Card",
                                    "Bank Transfer",
                                    "Monthly Installment"), selected = "Mobile"),
            actionButton("submit", "Submit Registration", class = "btn-success w-100")
           
          )
          
        ),
        
      
        
        # card(
        #   height = "300px",
        #   card_image(
        #     file = "www/bannerBottom.png",
        #     height = "auto"
        #   ),
        #   card_body(
        #     title = "Nature Image",
        #     "BioNutra product range "
        #   )
        # ),
        
        # layout_columns(
        #   col_widths = c(3,9),
        #   card(
        #     max_height = "250px",
        #     card_header(tags$span("Join YERP Program", style = "font-size: 20px; color: #00480F; font-weight: bold;")),
        #     card(
        #       max_height = "350px",
        #       textInput("first_name", "First Name*"),
        #       textInput("last_name", "Last Name*"),
        #       radioButtons("gender", "Gender*", 
        #                   choices = c("Male", "Female")),
        #       dateInput("date_of_birth", "Date of Birth*", min = "1950-01-01", max = Sys.Date(), value = "1970-06-18" ),
        #       textInput("email", "Email Address*"),
        #       textInput("phone", "Phone Number*"),
        #       selectInput("region", "Select Region*", choices = tanzania_regions),
        #       selectInput(inputId = "country", label = "Select Country*", 
        #                   choices = c("Select" = "", sort(countrycode::codelist$country.name.en))),
        #       numericInput("height", "Height (cm)*", value = NA),
        #       numericInput("weight", "Weight (kg)*", value = NA),
        #       selectInput("health_conditions", "Health Conditions*",
        #                   multiple = TRUE,
        #                   choices = c("Select" = "", "Diabetes", "High Blood Pressure", "Obesity", "General Nutrition", 
        #                               "Cancer", "Kidney Disease", "Metabolic", "Others")),
        #       selectInput("diet_plan", "Diet Plan*",
        #                   choices = c(
        #                               "Annual",
        #                               "Semi-Annual",
        #                               "Quarterly")),
        #       selectInput("payment_method", "Payment Method*",
        #                   choices = c("Select" = "", 
        #                               "Mobile",
        #                               "Credit Card",
        #                               "Bank Transfer",
        #                               "Monthly Installment")),
        #       actionButton("submit", "Submit Registration", class = "btn-success w-100")
        #     )
        #   ),
        #   # card(
        #   #   max_height = "200px",
        #     # card_header("Recent Registrations"),
        #     DTOutput("registrations_table"),
        #     # div(
        #     #   style = "margin-top: 15px",
        #     #   actionButton("stop_btn", "Stop Selected", class = "btn-danger"),
        #     #   actionButton("start_btn", "Start Selected", class = "btn-success")
        #     # )
        #   # )
        #   
        #   
        # )
      ),
      
    )
    
  ),
  
  # Wellbeing ----
  nav_panel(
    "WELLBEING",
    layout_sidebar(
      sidebar = sidebar(
        # title = "Analysis Controls",
        
        card(
          card_header("Quick Tips",  class = "bg-primary text-white"),
          "This web app allow users to Visualize key statistical values of disease based on clients treated at BioNutra"
        ),
        
        # How to Use card
        card(
          card_header("HOW TO USE?",  class = "bg-primary text-white"),
          tags$div(
            style = "font-size: 0.9em; padding: 10px;",
            tags$p(
              tags$b("1. Select City"),
              tags$br(),
              "Use dropdown menu to select a city",
              selectInput("city", NULL, choices = c("All", cities))
            ),
            tags$p(
              tags$b("2. Choose disease"),
              tags$br(),
              "Use dropdown menu to Select a particular disease",
              selectInput("disease", NULL, choices = c("All", diseases)),
            ),
            tags$p(
              tags$b("3. Explore Data:"),
              tags$ul(
                style = "padding-left: 0px; margin-top: 5px;",
                tags$p("Slide the bar by holding vertex"),
                # selectInput("period", "Time Period:", choices = c("Last Month", "Last Quarter", "Last Year", "All Time")),
                sliderInput("cases", NULL, min = 0, max = 300, value = c(50,300), ticks = FALSE)
                
              )
            )
          )
        )
      ),
      
      layout_column_wrap(
        width = 1/5,
        value_box(
          title = "Total Cases",
          value = textOutput("total_cases"),
          showcase = bsicons::bs_icon("clipboard-pulse"),
          theme = "primary",
          class = "text-white"
        ),
        value_box(
          title = "Cases-dominant",
          value = textOutput("top_city"),
          # showcase = bsicons::bs_icon("building"),
          theme = "primary",
          class = "text-white"
        ),
        value_box(
          title = "Critical Conditions",
          value = textOutput("critical_conditions"),
          # showcase = bsicons::bs_icon("exclamation-triangle"),
          theme = "primary",
          class = "text-white"
        ),
        # value_box(
        #   title = "Average Cases per City",
        #   value = textOutput("avg_cases"),
        #   # showcase = bsicons::bs_icon("calculator"),
        #   theme = "primary",
        #   class = "text-white"
        # ),
        value_box(
          title = "Common Condition",
          value = textOutput("top_disease"),
          # showcase = bsicons::bs_icon("heart-pulse"),
          theme = "primary",
          class = "text-white"
        ),
        value_box(
          title = "Cities Reporting Cases",
          value = textOutput("active_cities"),
          # showcase = bsicons::bs_icon("geo-alt"),
          theme = "primary",
          class = "text-white"
        )
      ),
      
      layout_column_wrap(
        width = 1/3,
        heights_equal = "row",
        card(
          card_header("Disease Distribution by City",  class = "bg-primary text-white"),
          plotlyOutput("barplot", height = "400px")
        ),
        card(
          card_header("Health Conditions Trend",  class = "bg-primary text-white"),
          plotlyOutput("trendPlot", height = "300px")
        ),
        card(
          card_header("Geographic Distribution" ,  class = "bg-primary text-white"),
          leafletOutput("map", height = "400px")
        )
      ),
      
      layout_columns(
        # width = 1/2,
        col_widths = c(4,8),
        
        card(
          card_header("Disease share",  class = "bg-primary text-white"),
          plotOutput("treemap")
        ),
        
        card(
          card_header("Detailed Statistics",  class = "bg-primary text-white"),
          DTOutput("statsTable")
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("BioNutra Website", href = "https://bionutra.co.tz/")),
    nav_item(tags$a("BioNutra Blog", href = "https://bionutra.co.tz/blog/our-blog-1"))
  ),
  
  
  # Footer ----
  tags$footer(
    style = "background-color: #00480F; color: #C6E3CC; padding: 15px 0; margin-top: 20px;",
    div(
      class = "container",
      div(
        class = "row justify-content-center align-items-center",
        div(
          class = "col-md-3 text-center",
          # tags$i(class = "fas fa-globe", style = "color: #90EE90; margin-right: 8px;"),
          bs_icon("globe"), 
          tags$a(href = "http://www.bionutra.co.tz", 
                 "https://bionutra.co.tz/",
                 style = "color: #C6E3CC; text-decoration: none;")
        ),
        div(
          class = "col-md-3 text-center",
          # tags$i(class = "fas fa-envelope", style = "color: #90EE90; margin-right: 8px;"),
          bs_icon("envelope"),
          tags$a(href = "mailto:info@bionutra.co.tz", 
                 "info@bionutra.co.tz",
                 style = "color: #C6E3CC; text-decoration: none;")
        ),
        div(
          class = "col-md-3 text-center",
          # tags$i(class = "fas fa-phone", style = "color: #90EE90; margin-right: 8px;"),
          bs_icon("telephone"), 
          tags$a(href = "tel:+255760938119", 
                 "+255 760 938 119",
                 style = "color: #C6E3CC; text-decoration: none;")
        ),
        div(
          class = "col-md-3 text-center",
          p("© 2024 BioNutra. All rights reserved.",
            style = "margin: 0; font-size: 0.9em; color: #C6E3CC;")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  
## POPUP YERP ----
  observeEvent(input$show_popup, {
    showModal(
      modalDialog(
        title = span(bs_icon("info-circle-fill"), "Special Message",
                     style = "margin: 0; font-size: 1.2em; font-weight: 400; color: #C6E3CC;"),
        div(
          class = "message-text",
          bs_icon("lightning-charge-fill"), 
          "Why suffer? YERP has your solution! Don’t wait—grab your plan today!",
          style = "font-size: 2.0em;"
        ),
        easyClose = TRUE,
        footer = tagList(
          actionButton(
            "close", 
            label = span( 
              bs_icon("x-circle-fill"),
              "Close"
            ),
            class = "btn-close-custom"
          )
        ),
        size = "l",
        class = "custom-popup"
      )
    )
  })
  
  observeEvent(input$close, {
    removeModal()
  })
  
  
  
  ## wellbeing Data ----
  filtered_data <- reactive({
    df <- disease_counts
    if (input$city != "All") {
      df <- df %>% filter(City == input$city)
    }
    if (input$disease != "All") {
      df <- df %>% filter(Disease == input$disease)
    }
    df
  })
  
  city_data <- reactive({
    city_summary <- filtered_data() %>%
      group_by(City) %>%
      summarize(Total = sum(Count), .groups = "drop")
    
    city_coords %>%
      left_join(city_summary, by = "City") %>%
      replace_na(list(Total = 0))
  })
  
  output$total_cases <- renderText({
    format(sum(filtered_data()$Count), big.mark = ",")
  })
  
  output$top_city <- renderText({
    filtered_data() %>%
      group_by(City) %>%
      summarize(Total = sum(Count), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice(1) %>%
      pull(City)
  })
  
  output$critical_conditions <- renderText({
    condition_summary <- filtered_data() %>%
      group_by(Disease) %>%
      summarize(Total = sum(Count), .groups = "drop")
    
    critical_count <- sum(condition_summary$Total > mean(condition_summary$Total))
    paste0(critical_count, " conditions")
  })
  
  output$avg_cases <- renderText({
    avg <- filtered_data() %>%
      group_by(City) %>%
      summarize(Total = sum(Count), .groups = "drop") %>%
      pull(Total) %>%
      mean()
    format(round(avg, 1), big.mark = ",")
  })
  
  output$top_disease <- renderText({
    filtered_data() %>%
      group_by(Disease) %>%
      summarize(Total = sum(Count), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice(1) %>%
      pull(Disease)
  })
  
  output$active_cities <- renderText({
    n_cities <- filtered_data() %>%
      group_by(City) %>%
      summarize(Total = sum(Count), .groups = "drop") %>%
      filter(Total > 0) %>%
      nrow()
    
    paste0(n_cities, " / ", length(cities))
  })
  
  output$barplot <- renderPlotly({
    # Summarize total cases per city
    city_summary <- filtered_data() %>%
      group_by(City) %>%
      summarize(
        Total = sum(Count),
        # Get top 3 diseases for tooltip
        Top_Diseases = paste(
          paste0(
            head(arrange(summarize(cur_group(), 
                                   Disease = Disease, 
                                   Cases = Count), 
                         desc(Cases))$Disease, 3),
            collapse = "<br>• "
          )
        ),
        .groups = "drop"
      )
    
    p <- ggplot(city_summary, 
                aes(x = reorder(City, Total), 
                    y = Total,
                    text = paste0(
                      "City: ", City,
                      "<br>Total Cases: ", format(Total, big.mark = ","),
                      "<br><br>Top Diseases:<br>• ", Top_Diseases
                    ))) +
      geom_col(fill = "#00480F", alpha = 0.8) +
      theme_minimal() +
      labs(x = "City", 
           y = "Total Cases") +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(labels = comma) +
      coord_flip()
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(align = "left"))
  })
  
  output$map <- renderLeaflet({
    data <- city_data()
    pal <- colorNumeric("YlOrRd", domain = data$Total)
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        popup = ~paste0(
          "<strong>", City, "</strong><br>",
          "Total Cases: ", format(Total, big.mark = ",")
        ),
        radius = ~sqrt(Total) * 2,
        color = ~pal(Total),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2
      ) %>%
      setView(lng = 35.7419, lat = -6.1730, zoom = 6)
  })
  
  output$trendPlot <- renderPlotly({
    disease_summary <- filtered_data() %>%
      group_by(Disease) %>%
      summarize(Total = sum(Count), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      mutate(Severity = cut(Total, 
                            breaks = 3,
                            labels = c("Low", "Medium", "High")))
    
    p <- ggplot(disease_summary, 
                aes(x = reorder(Disease, Total), 
                    y = Total,
                    fill = Severity,
                    text = paste0("Disease: ", Disease,
                                  "<br>Cases: ", format(Total, big.mark = ","),
                                  "<br>Severity: ", Severity))) +
      geom_col() +
      scale_fill_manual(values = c("Low" = "#90EE90", 
                                   "Medium" = "#FFB347", 
                                   "High" = "#FF6B6B")) +
      theme_minimal() +
      labs(x = "Health Condition", y = "Total Cases") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      coord_flip()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  stats_data <- reactive({
    filtered_data() %>%
      group_by(Disease) %>%
      summarize(
        Total_Cases = sum(Count),
        Avg_per_City = round(mean(Count), 1),
        Max_in_City = max(Count),
        Cities_Affected = n_distinct(City),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Cases))
  })
  
  output$statsTable <- renderDT({
    
    stats_data() |> 
      datatable(  options = list(
        pageLength = 5,
        dom = 'tp',
        scrollX = TRUE
      ),
      rownames = FALSE)
  })
  
  output$treemap = renderPlot({
    
    stats_data() |> 
      ggplot(aes(area = Total_Cases, fill = Disease)) +
      geom_treemap() +
      geom_treemap_text(aes(label = Disease), grow = TRUE, reflow = TRUE, padding.x = unit(6, "mm"), padding.y = unit(6, "mm"))+
      theme_void()+
      theme(legend.position = "none")+
      ggsci::scale_fill_lancet()
    
  })
  
  
  
  observeEvent(input$join_yerp, {
    showModal(modalDialog(
      title = "Join YERP Program",
      "Thank you for your interest! Please contact us using the information below to get started.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  # YERP DATABASE ----
  
  
  # Function to read registrations from database
  get_registrations <- function() {
    dbGetQuery(db, "SELECT * FROM yerp_members ORDER BY timestamp DESC")
  }
  
  # Handle form submission
  observeEvent(input$submit, {
    # Validate inputs
    required_fields <- c(input$first_name, input$last_name, input$gender,
                         input$email, input$phone, input$region,
                         input$country, input$height, input$weight,
                         input$diet_plan, input$payment_method)
    
    if (any(required_fields == "" | is.null(required_fields) | is.na(required_fields))) {
      showModal(modalDialog(
        title = "Error",
        "Please fill in all required fields marked with *",
        easyClose = TRUE
      ))
      return()
    }
    
    # Insert new registration into database
    tryCatch({
      dbExecute(db,
                "INSERT INTO yerp_members (
          timestamp, first_name, last_name, gender, date_of_birth, email,
          phone, region, country, height, weight, health_conditions,
          diet_plan, payment_method
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(
                  as.character(Sys.time()),
                  input$first_name,
                  input$last_name,
                  input$gender,
                  as.character(input$date_of_birth),
                  input$email,
                  input$phone,
                  input$region,
                  input$country,
                  input$height,
                  input$weight,
                  paste(input$health_conditions, collapse = ", "),
                  input$diet_plan,
                  input$payment_method
                )
      )
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        "Your YERP registration has been submitted successfully! Our team will contact you shortly.",
        easyClose = TRUE
      ))
      
      # Reset form
      updateTextInput(session, "first_name", value = "")
      updateTextInput(session, "last_name", value = "")
      updateSelectInput(session, "gender", selected = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "phone", value = "")
      updateSelectInput(session, "region", selected = "")
      updateSelectInput(session, "country", selected = "")
      updateNumericInput(session, "height", value = NA)
      updateNumericInput(session, "weight", value = NA)
      updateSelectInput(session, "health_conditions", selected = character(0))
      updateSelectInput(session, "diet_plan", selected = "")
      updateSelectInput(session, "payment_method", selected = "")
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Failed to save registration. Please try again.",
        easyClose = TRUE
      ))
    })
  })
  
 
  # Render the datatable
  output$registrations_table <- renderDT({
    
    get_registrations() |> 
      mutate(
      date = as.Date(timestamp),
      hour = hour(ymd_hms(timestamp)),
      minute = minute(ymd_hms(timestamp))
      ) |> 
      dplyr::select(date, last_name, gender, region, height, weight) |>
      datatable(
        selection = 'multiple',
        options = list(
                pageLength = 5,
                dom = 'tp',
                scrollX = FALSE
              ),
        rownames = FALSE
              ) |> 
      formatStyle(
        'gender',
        backgroundColor = styleEqual(
          c("Male", "Female"),
          c('#71BA80', '#00480F')
        )
      )
  })
  
  # Stop button handler
  observeEvent(input$stop_btn, {
    if (!is.null(input$process_table_rows_selected)) {
      current_data <- rv()
      current_data$Status[input$process_table_rows_selected] <- "Stopped"
      current_data$LastUpdated[input$process_table_rows_selected] <- Sys.time()
      rv(current_data)
    }
  })
  
  # Start button handler
  observeEvent(input$start_btn, {
    if (!is.null(input$process_table_rows_selected)) {
      current_data <- rv()
      current_data$Status[input$process_table_rows_selected] <- "Running"
      current_data$LastUpdated[input$process_table_rows_selected] <- Sys.time()
      rv(current_data)
    }
  })

  
  ## clickable link for joining YERP
  
  output$qr_code <- renderUI({
    # Make sure to install the qrcode package first: install.packages("qrcode")
    library(qrcode)
    qr <- qr_code("https://yourwebsite.com/signup")
    
    tags$img(
      src = qr,
      alt = "QR Code for signup",
      style = "width: 150px; height: 150px;"
    )
  })
  
  
  ## clickable Popupus for program features----
  
  modal_content <- function(title, icon, items) {
    showModal(modalDialog(
      title = NULL,
      HTML(sprintf('
        <div class="text-center">
          %s
          <h3>%s</h3>
        </div>
        <ul>
          %s
        </ul>
      ', as.character(bs_icon(icon)), title, paste(sprintf("<li>%s</li>", items), collapse = ""))),
      footer = modalButton("Close"),
      size = "m",
      easyClose = TRUE
    ))
  }
  
  observeEvent(input$mealplans, {
    modal_content(
      "Personalized Diet Plans for Optimal Health",
      "basket",
      c(
        "<b>Metabolic Efficiency:</b> Tailored to restore metabolic balance by prioritizing real, unprocessed foods that don't spike insulin or drive fat storage.",
        "<b>Avoiding the 'Fructose Trap':</b> Eliminates hidden sugars and processed carbs to keep the liver healthy and free of metabolic overload.",
        "<b>Feed Your Gut, Not Just Your Mouth:</b> Plans emphasize fiber-rich foods to nourish the gut microbiome, reducing inflammation and improving metabolic signaling.",
        "<b>Long-Term Health Investment:</b> Designed to reverse chronic metabolic diseases instead of masking symptoms with short-term fixes."
      )
    )
  })
  
  observeEvent(input$duration, {
    modal_content(
      "Flexible Meal Plans for Busy Lifestyles",
      "calendar-week",
      c(
        "<b>Ditch the Drive-Thru:</b> Provides quick, wholesome options to replace nutrient-depleting fast food.",
        "<b>Food on Your Schedule:</b> Meals structured to align with circadian rhythms, promoting better insulin sensitivity during the day.",
        "<b>One Plan, Many Lives:</b> Family-friendly and scalable, making it easier to implement healthy changes without disruption.",
        "<b>Time is Money, and So is Health:</b> Helps save both with efficient planning and nutrient-packed meals that keep you full longer."
        
      )
    )
  })
  
  observeEvent(input$coaching, {
    modal_content(
      "Year-round personalized Nutrition Coaching",
      "person-video3",
      c(
        "<b>Metabolic Sherpa:</b> Your guide to navigating the modern food landscape, avoiding 'fake food' traps, and understanding what truly nourishes.",
        "<b>Metrics that Matter:</b> Regular checks on real markers of health like waist size, energy levels, and focus, not just the number on the scale.",
        "<b>Unlearn the Myths:</b> Debunks diet industry fallacies, teaching you that not all calories are created equal.",
        "<b>Support for the Sugar-Addicted Brain:</b> Behavioral coaching to help rewire cravings and overcome the dopamine-driven addiction to processed food."
        
      )
    )
  })
  
  observeEvent(input$brain, {
    modal_content(
      "Enhance focus, memory and Mental activeness",
      "lightning-charge",
      c(
        "<b>Food for Thought:</b> Incorporates nutrients like omega-3s and antioxidants to power up your brain's biochemistry.",
        "<b>Kill the Cortisol:</b> Lowers stress hormone levels through a diet rich in whole foods and devoid of high-fructose junk.",
        "<b>Fuel Without the Crash:</b> Stabilizes blood sugar to avoid the highs and lows that derail focus and productivity.",
        "<b>Brain-Gut Connection:</b> Prioritizes gut health to reduce inflammation, enhancing clarity and mental sharpness."
        
      )
    )
  })
  
  observeEvent(input$gut, {
    modal_content(
      "Support immune system and Maintain Overall health",
      "heart-pulse",
      c(
        "<b>Micronutrient Mastery:</b> Packed with vitamins and minerals that support mitochondrial function and immune cell regeneration.",
        "<b>Ditch the Inflammatory Factory:</b> Minimizes foods that drive systemic inflammation, keeping your immune system at the ready.",
        "<b>Season-Proof Your Health:</b> Plans adapt to fight seasonal immune threats with nature’s pharmacy—whole foods.",
        "<b>Sick Less, Thrive More:</b> Focuses on overall health rather than reacting to illnesses when they strike."
      )
    )
  })
  
  observeEvent(input$liver, {
    modal_content(
      "Reset and detoxify the body with our Electrical Foods",
      "shield-plus",
      c(
        "<b>Cellular Recharge:</b> Alkaline, nutrient-dense foods act as the “recharge” for your mitochondria—the body’s power plants.",
        "<b>Drain the Fructose Swamp:</b> Detoxifies the liver and flushes out excess fat caused by processed sugars.",
        "<b>Inflammation Vacuum:</b> Reduces oxidative stress and cellular inflammation with foods rich in antioxidants and polyphenols.",
        "<b>The Body’s Reset Button:</b> Focuses on foods that naturally help restore hormonal balance, especially insulin and leptin."
        
      )
    )
  })
  
  observeEvent(input$insulin, {
    modal_content(
      "Activate metabolic processes & Improve insulin sensitivity",
      "shield-plus",
      c(
        "<b>Turn Fat into Fuel:</b> Encourages the body to burn stored fat instead of relying on constant sugar intake",
        "<strong>Stop the Insulin Flood:</strong> Focuses on low-glycemic foods to reduce insulin spikes that lead to fat storage and metabolic damage.",
        "<strong>Reverse the Damage:</strong> Restores insulin sensitivity, a cornerstone of reversing Type 2 diabetes and metabolic syndrome.",
        "<strong>Metabolic Harmony:</strong> Provides the building blocks for a healthy liver, a balanced gut, and sustained energy throughout the day."
        
      )
    )
  })
  
  

  
  
}

shinyApp(ui, server)

## Retrieve data ----

# db <- dbConnect(SQLite(), "yerp_members.db")
# db |> DBI::dbListTables()
# db |> tbl("yerp_members" )

