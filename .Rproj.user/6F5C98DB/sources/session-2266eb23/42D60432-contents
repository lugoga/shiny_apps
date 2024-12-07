require(shiny)
require(bslib)
require(tidyverse)
require(gt)
require(DT)
require(plotly)
require(treemapify)

theme_set(new = theme_minimal(base_size = 18))
all_themes <- c(
  "cerulean", "cosmo", "cyborg", "darkly", "flatly", 
  "journal", "litera", "lumen", "lux", "materia", 
  "minty", "morph", "pulse", "quartz", "sandstone", 
  "simplex", "sketchy", "slate", "solar", "spacelab", 
  "superhero", "united", "vapor", "yeti", "zephyr"
)

wajumbe = rio::import("wajumbe.xlsx")

# Create a data frame for polynomial orders
polynomial_df <- data.frame(
  Order = 1:10,
  Name = c("Linear","Quadratic","Cubic","Quartic","Quintic","Sextic", "Septic", "Octic","Nonic","Decic")
)

# activity_data = read_csv("D:/NCMC/webApps/nirT/activity_data.csv")
activity_data = read_csv("activity_data.csv") #|> filter(area == "Mainland")

sectors = activity_data |> distinct(sector)
energy_data = activity_data |>   filter(sector == "Energy" )
energy_datacat = energy_data |> distinct(data_category) |> pull()

ui = page_navbar(
  # theme = bs_theme(version = 5, bootswatch = "minty"),
  theme = bs_theme(),
  selectInput("theme", NULL, choices = all_themes, selected = "superhero"),
  
  # Title 
  title = div(
    style = "display: flex; align-items: center; justify-content: center; gap: 15px;",
    tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/3/38/Flag_of_Tanzania.svg", height = "40px"),
    tags$h3("National Inventory Report | Activity Data"),
    
  ),
  # bg = "#2D89C8",
  # inverse = TRUE,
  nav_spacer(),
  nav_panel(title = "HOME"),
  nav_panel(
    title = "ENERGY",
    layout_columns(
      col_widths = c(2,2,4,4),
      card(
        card_header("OVERVIEW"),
        p("Greenhouse gas (GHG) emissions from the energy sector are based on energy process stages including generation, transformation and use. The combustion of coal, natural gas and petroleum products such as diesel, gasoline, Liquefied Petroleum Gas (LPG) and Kerosene takes place in both stationary and mobile operations in various socio-economic activities in which contribute to GHG emissions. In addition to emission from combustion, unintended emissions are also generated from upstream coal and natural gas exploration, production, and processing. Energy-related emissions account for approximately 7.8% of Tanzania’s total national GHG emissions (USAID, 2018)."),
        p("Tanzania aims to reduce greenhouse gas emissions by 30 - 35% compared to the Business-As-Usual (BAU) scenario by 2030, which is expected to result in a reduction of approximately 138 - 153 million tons of Carbon dioxide equivalent (MtCO2e) in gross emissions will be reduced. Emissions reduction from the energy sector will enable the country to embark on a low emission growth pathway, while achieving climate resilience and the anticipated sustainable development (URT, 2021). Among the initiatives taken by the government was phasing out diesel/HFO power plants from the national grid system since 2021 (URT, 2020)."),
        p("The national energy balance reveals that biomass primarily in the form of charcoal and firewood which contributes about 85 percent of the total national energy consumption. On the other hand, Petroleum products make up about 9.3 percent of the total energy consumed while electricity represents 4.5 percent and 1.2 percent from coal and renewable energies (URT, 2015). Currently, the entire production of the country’s natural gas is utilized as follows: 81.57% for domestic power generation,18.24% for industrial use,0.17% for CNG vehicles, and 0.03 for households, commercial interprises and institutions (URT, 2023). "),
        DT::dataTableOutput(outputId = "energy_treemap"),
        p("The provided information are the huge deposits of natural gas in Tanzania, the country has not proven oil reserves yet, the country continues to rely on the importation of petroleum products (Kitonga et al., 2015). The second national communication proves that the combined heat and power generation (CHP) or cogeneration is only carried out in sugar plants that utilize waste bagasse to generate electricity and process heat (URT, 2014).")
        ),
      card(
        card_header("HOW TO EXPLORE"),
        
            tags$div(
            style = "font-size: 0.9em; padding: 10px;",
            tags$p(
              tags$b("1. Select category"),
              tags$br(),
              "Use the dropdown menu to choose a category.",
              selectInput(inputId = "energy_cat", label = NULL, choices = energy_datacat)
            ),
            
            tags$p(
              tags$b("2. Select sub-category"),
              tags$br(),
              "Use the dropdown menu to choose a sub category.",
              uiOutput("subcategory_filter"),
            ),
            
            tags$p(
              tags$b("3. Choose an Area"),
              tags$br(),
              "Use the dropdown menu to choose either Mainland Tanzania or Zanzibar or both",
              radioButtons(inputId = "energy_area", label = NULL, choices = c("Mainland", "Zanzibar", "Tanzania"), selected = "Mainland"),
              
            ),
            
            tags$p(
              tags$b("4. Inventory Year"),
              tags$br(),
              "Adjust the time slider to choose specific years of interest",
              sliderInput(inputId = "energy_years", label = NULL, min = 1990, max = 2025, value = c(1994,2021), step = 1, ticks = FALSE),
            ),
            
            tags$p(
              tags$b("5. Fitting data to models"),
              tags$br(),
              "Adjust the number in slider to fit the data into the best-fit model.",
              sliderInput(inputId = "energy_orders", label = NULL, min = 1, max = 10, value = 3),           
              ),
            
            
            
            tags$p(
              tags$b("6. Inventory Year"),
              tags$br(),
              "Adjust the time slider to highlight a period of data gaps",
              sliderInput(inputId = "energy_gaps", label = NULL, min = 1990, max = 2021, value = c(1995,2012))
              ),
            
          
            tags$p(
              tags$b("7. Explore Data:"),
              tags$ul(
                style = "padding-left: 0px; margin-top: 5px;",
                tags$p("Visualize trends and explore the raw data to identifying an potential data error and gaps"),
              )
            ),
        ),
        ),
      card(
        card_header("ACTIVITY DATA"),
        tags$p(
          tags$b("TRENDs"),
          tags$br(),
          "Pay attention to the nature of the data on whether the value is increasing or decreasing or random over the period. Check also if the data cover the required period",
          plotOutput(outputId = "trend_plot_energy"),
        ),
        
        tags$p(
          tags$b("RAWQ DATA"),
          tags$br(),
          "By clinking on Data variable, you can arrange the data in ascending or descing order, and the values are color coded in increasing order",
          gt::gt_output(outputId = "energy_raw")
          )
        ),
      card(
        card_header("EMISSION DATA"),
       
        )
      )
    ),
  nav_panel(title = "AGRICULTURE", p("Second page content.")),
  nav_panel(title = "IPPU", p("Third page content.")),
  nav_panel(title = "LULCF", p("Third page content.")),
  nav_panel(title = "WASTE", p("Third page content.")),
  nav_panel(title = "WOOD", p("Third page content.")),
  nav_panel(
    title = "Task Force",
    layout_columns(
      col_widths = c(2,8),
      card(
        card_header("TAMKO",  class = "bg-primary text-white"),
        p("Tanzania’s National Inventory Report (NIR) is a critical tool for understanding and managing the country’s greenhouse gas (GHG) emissions."),
        p("The foundation for the NIR has been laid with the successful collection, compilation, and storage of activity data across key sectors, including energy, agriculture, land use, industrial processes, and waste management."),
        p("This data is crucial for quantifying GHG emissions, as it reflects the country’s economic activities that directly or indirectly contribute to emissions.")

        ),
      card(
        # max_height = "400px",
        card_header("Task Force", class = "bg-primary text-white"),
        gt::gt_output("members")
      ),
      card(
        card_header("HITIMISHO",  class = "bg-info text-white"),
        p("The preparation of this National Inventory Report (NIR) has been a collective effort, made possible through the collaboration of various institutions and stakeholders across both mainland Tanzania and Zanzibar."),
        p("We extend our deepest gratitude to all those who contributed their expertise, time, and resources to this significant undertaking. We wish to acknowledge the Vice President’s Office – Division of Environment for its leadership and overall coordination, which has ensured smooth collaboration between the mainland and Zanzibar.")
        )
    ),
    
    ),
 
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  ),
 
  
  # Footer
  tags$footer(
    style = "background-color: #f8f9fa; padding: 15px; text-align: center; margin-top: 20px; border-top: 1px solid #dee2e6;",
    "© SEMBA 2024 | All Rights Reserved"
  )
)


server = function(input, output, session) {
  
  
  observe({
    # Dynamically change the theme based on user input
    session$setCurrentTheme(
      bs_theme(bootswatch = input$theme)
    )
  })
  
  
  # energy ----
  # Final filtered data based on both filters
  cleaned_filtered_data <- reactive({
    req(input$energy_cat) # Ensure gear input is available
    energy_data |> filter(data_category == input$energy_cat)
  })
  
  
  # Generate third filter (data_type) based on the first filter
  output$subcategory_filter <- renderUI({
    selectInput(
      inputId = "input_data_types", 
      label = "Select Subcategory:",
      choices = cleaned_filtered_data() |> distinct(data_type) |> pull()
      )
  })
  
  
  # Final-final filtered data based on both filters
  final_filtered_data <- reactive({
    req(input$input_data_types) 
    cleaned_filtered_data() |> 
      filter(
        area == input$energy_area & 
          data_type == input$input_data_types &
          year >= input$energy_years[1] & year <= input$energy_years[2]
        ) 
  })
  
  
  # Reactive filtered data
  area_filtered_data <- reactive({
    if (input$energy_area == "Mainland") {
      final_filtered_data() |> 
        filter(area == "Mainland")
    } else if (input$energy_area == "Zanzibar") {
      final_filtered_data() |> 
        filter(area == "Zanzibar")
    } else if (input$energy_area == "Tanzania") {
      final_filtered_data() |>
        group_by(year) |> 
        summarise(data_value = sum(data_value, na.rm = TRUE), .groups = "drop")
    } else {
      NULL
    }
  })
  
  
  
  output$trend_plot_energy = renderPlot({
    
    area_filtered_data()  |>
      ggplot(aes(x = year, y = data_value))+
      geom_line()+
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y~poly(x,input$energy_orders), fill = "red", color = "red", alpha = .1)+
      labs(subtitle = paste0(polynomial_df |> slice(input$orders) |> pull(Name)," Polynomial "))+
      annotate(geom = "rect", xmin = input$energy_gaps[1], xmax = input$energy_gaps[2], ymin = min(final_filtered_data()$data_value, na.rm = TRUE), ymax = Inf, fill = "blue", alpha = .2) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::label_number(), name = paste0(final_filtered_data() |> slice(1) |> pull(data_category)," (", final_filtered_data() |> slice(1) |> pull(unit),")"))+
      theme(axis.title.x = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_line(linetype = 2, linewidth = .5))
  })
  
  
  
  output$energy_raw =  gt::render_gt({
    area_filtered_data() |> 
      select(-sector,-data_category, -data_type, -area) |>
      gt()|>
      cols_label(
        year = "Year",
        data_value = "Data",
        unit = "SI Unit",
        data_source = "Source"
      ) |>
      # cols_label_with(
      #   fn = ~ janitor::make_clean_names(., case = "all_caps")
      # )|>
      gt::opt_interactive(
        # use_search = TRUE,
        use_filters = FALSE,
        use_resizers = TRUE,
        use_highlight = TRUE,
        use_compact_mode = TRUE,
        use_text_wrapping = FALSE,
        use_page_size_select = TRUE
      ) |>
      fmt_number(decimals = 0) |>
      fmt_integer(year, use_seps = FALSE) |>
      # Add coloring for the numeric column
      data_color(
        columns = vars(data_value),
        colors = hcl.colors(n = 120, palette = "Spectral", rev = TRUE)
      )
    
  })
  
  
  ## energy-intro----
  
  output$energy_treemap = renderDT({
    palmerpenguins::penguins |> datatable()
  })
  
    ## Task force ----
  output$members = gt::render_gt({
    wajumbe |> 
      select(
        -gender, -Area, -phone, -middle
      ) |> 
      mutate(city = str_to_upper(city), address = str_to_upper(address)) |> 
      gt(rowname_col = "firstname") |> 
      # tab_header(title = "Task Force Members for GHG emission Estimation") |> 
      tab_stubhead(label = "Name") |> 
      fmt_email(columns = email) |> 
      # fmt_country(columns = Area) |> 
      cols_merge(
        columns = c(institution, address, city),
        pattern = "{1}<br>{2}<br>{3}<br>"
      ) |> 
      cols_merge(
        columns = c(firstname, surname),
        pattern = "{1}<br>{2}"
      ) |> 
      cols_label(
        institution = "Affiliation Mailing Adress",
        email = "Email"
      ) |> 
      # tab_style(
      #   style = cell_text(size = "x-small"),
      #   locations = cells_body(columns = c(institution,institution))
      # ) |>
      opt_align_table_header(align = "left")
  })
  
}


shinyApp(ui = ui, server = server)

# runApp("02_ui")


