library(tidyverse)
library(leaflet)
library(bslib)
require(bsicons)
library(forcats)
library(plotly)
library(DT)
library(scales)
require(treemapify)

# Data preprocessing
cities <- c("Arusha", "Dar es Salaam", "Mbeya", "Dodoma", "Mwanza", "Moshi", "Morogoro", "Tanga", "Others")
diseases <- c("Diabetes", "High Blood Pressure", "Obesity", "General Nutrition", 
              "Cancer", "Kidney Disease", "Metabolic", "Others")

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
#   primary = "#2E8B57", 
#   # secondary = "#C6E3CC",
#   "enable-shadows" = TRUE,
#   "card-border-radius" = "15px" 
# )

my_theme <- bs_theme(
  version = 5,
  primary = "#2E8B57",
  # Adjusting a few other properties to complement the primary color
  "enable-shadows" = FALSE,
  "body-bg" = "#C6E3CC",
  "border-radius" = "0.5rem",
  "card-border-radius" = "15px",
  base_font = font_google("Poppins")
) 

ui <- page_navbar(
  fillable = TRUE,
  title = span(
    img(src = "BioNutra.svg", height = "40px", style = "margin-right: 10px;"),
    "BioNutra"
  ),
  theme = my_theme,
  
  # Add Google Fonts link
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap",
      rel = "stylesheet"
    ),
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
    ")
  ),
  
  # Add Font Awesome
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
  ),
  
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap", 
              rel = "stylesheet"),
    tags$link(rel = "stylesheet", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    #   tags$style("
    #     /* Home panel specific styles */
    #     #Home .vertical-center {
    #       min-height: calc(100vh - 60px);
    #       display: flex;
    #       align-items: center;
    #       justify-content: center;
    #       flex-direction: column;
    #       background: #C6E3CC;
    #     }
    #     #Home .main-title {
    #       font-size: 3.8rem;
    #       font-weight: 600;
    #       color: #2E8B57;
    #       text-align: center;
    #       line-height: 1.4;
    #       margin-bottom: 1rem;
    #       text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
    #     }
    #     #Home .subtitle {
    #       font-size: 1.8rem;
    #       font-weight: 300;
    #       color: #2E8B57;
    #       text-align: center;
    #       margin-bottom: 4rem;
    #     }
    #     #Home .value-box {
    #       transition: transform 0.3s ease;
    #       background-color: white !important;
    #     }
    #     #Home .value-box:hover {
    #       transform: translateY(-5px);
    #     }
    #     #Home .wellbeing-container {
    #       margin-top: 2rem;
    #       width: 100%;
    #       max-width: 1200px;
    #       padding: 0 20px;
    #     }
    #     #Home .value-box-title {
    #       font-size: 1.1rem !important;
    #       font-weight: 500 !important;
    #       color: #2E8B57 !important;
    #     }
    #     #Home .value-box-value {
    #       font-size: 2.2rem !important;
    #       font-weight: 600 !important;
    #       color: #1a5c38 !important;
    #     }
    #     #Home .value-box p {
    #       color: #666 !important;
    #       font-size: 0.95rem !important;
    #     }
    #     #Home .value-box .value-box-showcase {
    #       color: #2E8B57 !important;
    #     }
    # 
    #     /* YERP panel specific styles */
    #     #YERP .pricing-cards .card {
    #       transition: transform 0.3s ease;
    #     }
    #     #YERP .pricing-cards .card:hover {
    #       transform: translateY(-5px);
    #     }
    #     #YERP .list-unstyled li {
    #       margin-bottom: 1rem;
    #       font-size: 1.1rem;
    #     }
    #     
    #     #Home {
    #   background-color: #f5f9f7;  /* Light mint green background */
    # }
    # #Home .vertical-center {
    #   min-height: calc(100vh - 60px);
    #   display: flex;
    #   align-items: center;
    #   justify-content: center;
    #   flex-direction: column;
    #   background: #C6E3CC;
    # }
    # 
    # /* Dashboard panel specific styles */
    # #Dashboard {
    #   background-color: #f8f9fa;  /* Light gray background */
    #   min-height: 100vh;
    #   padding: 20px;
    # }
    # 
    # /* YERP panel specific styles */
    # #YERP {
    #   background-color: #f5f9f7;  /* Light mint green background */
    #   min-height: 100vh;
    #   padding: 20px;
    # }
    # #YERP .pricing-cards .card {
    #   transition: transform 0.3s ease;
    #   background-color: white;
    # }
    # #YERP .pricing-cards .card:hover {
    #   transform: translateY(-5px);
    # }
    # #YERP .list-unstyled li {
    #   margin-bottom: 1rem;
    #   font-size: 1.1rem;
    # }
    # 
    # /* Card styles */
    # .card {
    #   background-color: white;
    #   box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    #   
    #   
    # }
    #     
    #   ")
  ),
  
  nav_panel(
    title = "HOME",
    div(
      class = "vertical-center",
      div(
        class = "container",
        div(
          class = "row justify-content-center",
          div(
            class = "col-md-10 text-center",
            h1(
              "YEAR-LONG EATING RIGHT PROGRAM (YERP)", class = "main-title"
            ),
            h4("Stay Healthy and Active Year-round with Our Electrical Foods",   class = "main-title"),
          )
        ),
        tags$br(),
        tags$br(),
        div(
          class = "wellbeing-container",
          # First row
          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",
            value_box(
              title = span("Program Duration", style = "color: #008B8B;"),
              value = span("12 Months", style = "color: #1a5c38;"),
              showcase = bsicons::bs_icon("calendar-check-fill", size = "3em", style = "color: #2E8B57;"),
              p("Transform your health with our year-round support system", 
                style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            value_box(
              title = span("Personalized Plans", style = "color: #008B8B;"),
              value = span("Diet Plans", style = "color: #1a5c38;"),
              showcase = bsicons::bs_icon("journal-medical", size = "3em", style = "color: #20B2AA;"),
              p("Quarterly customized nutrition plans tailored to your needs",
                style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            )
            ,
            value_box(
              title = span("Program Success Rate", style = "color: #008B8B;"),
              value = span("90%", style = "color: #1a5c38;"),
              showcase = bsicons::bs_icon("graph-up-arrow", size = "3em", style = "color: #3CB371;"),
              p("Participants achieving their desired health goals",
                style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            value_box(
              title = span("Expert Support Access", style = "color: #008B8B;"),
              value = span("24/7", style = "color: #1a5c38;"),
              showcase = bsicons::bs_icon("people-fill", size = "3em", style = "color: #228B22;"),
              p("Round-the-clock access to nutrition professionals",
                style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            )
          ),
          # second row
          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",
            value_box(
              title = span("Nutrition Workshops", style = "color: #008B8B;"),
              value = span("12 Sessions", style = "color: #1a5c38;"),
              showcase = bsicons::bs_icon("easel2-fill", size = "3em", style = "color: #006400;"),
              p("Monthly interactive workshops on healthy eating and lifestyle",
                style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            value_box(
              title = span("Recipe Database", style = "color: #008B8B;"),
              value = span("200+", style = "color: #1a5c38;"),
              showcase = bsicons::bs_icon("journal-bookmark-fill", size = "3em", style = "color: #32CD32;"),
              p("Access to exclusive healthy recipes and meal ideas",
                style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            
            value_box(
              title = span("Health Tracking", style = "color: #008B8B;"),
              value = span("Weekly", style = "color: #1a5c38;"),
              showcase = bsicons::bs_icon("clipboard2-pulse-fill", size = "3em", style = "color: #008B8B;"),
              p("Regular monitoring of your health metrics and progress",
                style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            ),
            value_box(
              title = span("Community Support", style = "color: #008B8B;"),
              value = span("1000+", style = "color: #1a5c38;"),
              showcase = bsicons::bs_icon("geo", size = "5rem", style = "color: #4CAF50;"),
              p("Join our growing community of health-conscious individuals",
                style = "color: #666;"),
              fill = TRUE,
              theme_color = "#008B8B"
            )
          )
        )
      )
    )
  ),
  nav_panel(
    title = "YERP",
    layout_column_wrap(
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-md-10 text-center",
          h3(
            str_to_upper("A Science-backed Personalized Diet Program"), class = "main-title"
          ),
          h1("for Natural Healing & Prevention",   class = "main-title"),
        )
      ),
      
      # div(class = "text-center",
      #     # tags$img(src = "calendar.svg", height = "150px", class = "img-fluid"),
      #     # bsicons::bs_icon(name = "diagram-3", size = "5em")
      # ),
      # layout_sidebar(
      #   sidebar = card(
      #       gap = "1rem",
      #       sliderInput(inputId = "fin", "Financial", min = 0,max = 10, value = 7),
      #       sliderInput(inputId = "emo", "Emotional", min = 0,max = 10, value = 7),
      #       sliderInput(inputId = "int", "Intellectual", min = 0,max = 10, value = 7),
      #       sliderInput(inputId = "lon", "Longevity", min = 0,max = 10, value = 7),
      #       sliderInput(inputId = "env", "Environmental", min = 0,max = 10, value = 7),
      #       sliderInput(inputId = "occ", "Occupational", min = 0,max = 10, value = 7),
      #       sliderInput(inputId = "nut", "Nutritional", min = 0,max = 10, value = 7),
      #       sliderInput(inputId = "phy", "Physical", min = 0,max = 10, value = 7)
      # 
      #   ),
      #   # plotOutput("wellbeing"),
      #   tags$h3("Wellpeing")
      # ),
      # )
    ),
    layout_columns(
      col_widths = c(3,3,6),
      # min_height = "300px",
      card(
        
        card_header(tags$span("NEED TO KNOW YERP!!!", style = "font-size: 24px; color: #2E8B57; font-weight: bold;")),
        card_body("The program sets your body to naturally Heal, Nourish and Prevent itself from a range of metabolic health conditions year-round through customized meal plans.")
      )
    ),
    
    layout_columns(
      col_widths = c(3,7,2),
      card(
        max_height = "100%",
        card_header(tags$span("Program Features", style = "font-size: 24px; color: #2E8B57; font-weight: bold;")),
        tags$ul(class = "list-unstyled",
                tags$li(tags$i(class = "fas fa-calendar-alt text-primary me-2"), "Customized 4 Meal Plans Annually"),
                tags$li(tags$i(class = "fas fa-clock text-success me-2"), "Each Plan Lasts 7-10 Days per Quarter"),
                tags$li(tags$i(class = "fas fa-user-md text-info me-2"), "Year-round Nutrition Coaching"),
                tags$li(tags$i(class = "fas fa-brain text-warning me-2"), "Support your Brain"),
                tags$li(tags$i(class = "fas fa-bacteria text-danger me-2"), "Feed your Gut"),
                tags$li(tags$i(class = "fas fa-heart text-primary me-2"), "Protect the Liver"),
                div(class = "text-center mt-4",
                    # tags$img(src = "calendar.svg", height = "100px"),
                    bsicons::bs_icon("heart-pulse-fill", size = "6em"),
                    bsicons::bs_icon("heart-pulse-fill", size = "6em"),
                    bsicons::bs_icon("heart-pulse-fill", size = "6em")
                    
                )
        )
      ),
      
      card(
        max_height = "100%",
        card_header(tags$span("Billing Plans", style = "font-size: 24px; color: #2E8B57; font-weight: bold;")),
        div(
          class = "table-responsive p-3",
          tags$div(class = "pricing-cards",
                   layout_column_wrap(
                     width = 1/3,
                     card(
                       card_header(
                         tags$i(class = "fas fa-star text-warning fa-2x"),
                         # h4("Annual Plan", class = "mt-2"),
                         tags$span("Annual Plan", style = "font-size: 20px; color: #2E8B57; font-weight: normal;")
                       ),
                       tags$div(class = "text-center p-3",
                                h3("1,000,000 TZS"),
                                p("250,000 TZS per quarter"),
                                p("(Paid Upfront)"),
                                # tags$img(src = "calendar.svg", height = "80px"),
                                bsicons::bs_icon(name = "cash-coin", size = "5em")
                       )
                     ),
                     card(
                       card_header(
                         tags$i(class = "fas fa-gem text-info fa-2x"),
                         # h4("Semi-annual Plan", class = "mt-2"),
                         tags$span("Semi-Annual Plan", style = "font-size: 20px; color: #2E8B57; font-weight: normal;")
                       ),
                       tags$div(class = "text-center p-3",
                                h3("1,199,500 TZS"),
                                p("299,625 TZS per quarter"),
                                p("(Two Payments)"),
                                # tags$img(src = "calendar.svg", height = "80px"),
                                bsicons::bs_icon(name = "wallet", size = "5em")
                       )
                     ),
                     card(
                       card_header(
                         tags$i(class = "fas fa-award text-success fa-2x"),
                         # h4("Quarterly Plan", class = "mt-2"),
                         tags$span("Quarterly Plan", style = "font-size: 20px; color: #2E8B57; font-weight: normal;")
                       ),
                       tags$div(class = "text-center p-3",
                                h3("1,410,000 TZS"),
                                p("352,500 TZS per quarter"),
                                p("(Four Payments)"),
                                # tags$img(src = "calendar.svg", height = "80px"),
                                bsicons::bs_icon(name = "wallet2", size = "5em")
                       )
                     )
                   )
          )
        )
      ),
      
      card(
        max_height = "100%",
        card_header( tags$span("Joining", style = "font-size: 20px; color: #2E8B57; font-weight: bold;")),
        tags$div(
          class = "d-grid gap-2 p-3",
          # tags$img(src = "benefits-illustration.png", height = "150px", class = "mx-auto d-block mb-3"),
          actionButton("join_yerp", "Join the YERP Program", 
                       class = "btn-lg btn-primary mb-3"),
          tags$div(
            class = "text-center",
            tags$h3("Scan the qrcode to register"),
            tags$img(src = "joining.svg", height = "100px", class = "mt-3")
            # bsicons::bs_icon(name = "box2-heart", size = "5em")
          )
        )
      )
    )
  ),
  
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
        width = 1/6,
        value_box(
          title = "Total Cases",
          value = textOutput("total_cases"),
          # showcase = bsicons::bs_icon("clipboard-pulse"),
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
        value_box(
          title = "Average Cases per City",
          value = textOutput("avg_cases"),
          # showcase = bsicons::bs_icon("calculator"),
          theme = "primary",
          class = "text-white"
        ),
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
          card_header("Disease share",  class = "bg-success text-white"),
          plotOutput("treemap")
        ),
        
        card(
          card_header("Detailed Statistics",  class = "bg-success text-white"),
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
  
  # Add this at the end of your UI, just before the closing parenthesis of page_navbar()
  
  tags$footer(
    style = "background-color: #2E8B57; color: white; padding: 15px 0; margin-top: 20px;",
    div(
      class = "container",
      div(
        class = "row justify-content-center align-items-center",
        div(
          class = "col-md-3 text-center",
          tags$i(class = "fas fa-globe", style = "color: #90EE90; margin-right: 8px;"),
          tags$a(href = "http://www.bionutra.co.tz", 
                 "https://bionutra.co.tz/",
                 style = "color: white; text-decoration: none;")
        ),
        div(
          class = "col-md-3 text-center",
          tags$i(class = "fas fa-envelope", style = "color: #90EE90; margin-right: 8px;"),
          tags$a(href = "mailto:info@bionutra.co.tz", 
                 "info@bionutra.co.tz",
                 style = "color: white; text-decoration: none;")
        ),
        div(
          class = "col-md-3 text-center",
          tags$i(class = "fas fa-phone", style = "color: #90EE90; margin-right: 8px;"),
          tags$a(href = "tel:+255760938119", 
                 "+255 760 938 119",
                 style = "color: white; text-decoration: none;")
        ),
        div(
          class = "col-md-3 text-center",
          p("© 2024 BioNutra. All rights reserved.",
            style = "margin: 0; font-size: 0.9em; color: #90EE90;")
        )
      )
    )
  )
)

server <- function(input, output) {
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
      geom_col(fill = "#2E8B57", alpha = 0.8) +
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
  
  
}

shinyApp(ui, server)