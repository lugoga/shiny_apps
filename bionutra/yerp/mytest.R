require(shiny)
require(bslib)
require(bsicons)
require(tidyverse)


ui = page_sidebar(
  title = "Testing", theme = bs_theme(version = 5, bootswatch = "flatly"),fillable = FALSE,
  sidebar = sidebar(
    title = "UMUHIMU",
    ),
  layout_columns(
    col_widths = c(3,3,6),
    fillable = T,
    card(
      card_header("CHakula", class = "primary", style = "color: steelblue; font-size: 20px; font-weight: bold;"),
      card_body("sunt repe cum sit hillate cus et dolore nonsequatus aceaqui quia etus ere expe nis sed ma doluptaspero quatem nimet estium similla ccatis dolorem re alit que litis di con ni nonsequia nis saepelicat et alita quam quid minullant aut hiliatis de nieni derum dolorem as aliqui blaciati sum nonsenis dent ati consequo officitiae. Ed quaecum volliciam que conseque etur aut ommoluptaqui consed quiandunt quas est, autem quia custrum volupicium imus."),
      card_body("sunt repe cum sit hillate cus et dolore nonsequatus aceaqui quia etus ere expe nis sed ma doluptaspero quatem nimet estium similla ccatis dolorem re alit que litis di con ni nonsequia nis saepelicat et alita quam quid minullant aut hiliatis de nieni derum dolorem as aliqui blaciati sum nonsenis dent ati consequo officitiae. Ed quaecum volliciam que conseque etur aut ommoluptaqui <strong>consed quiandunt quas est</strong>, autem quia custrum volupicium imus.")
      ),
    card(
      card_header("Taarifa", class = "primary", style = "color: steelblue; font-size: 20px; font-weight: bold;"),
      layout_columns(
        col_widths = c(6,6),
        dateInput(inputId = "date", label = "Date"),
        textInput(inputId = "name", "Name of Site"),
        selectInput("miezi", "Choose months", c("select", month.abb)),
        radioButtons("years", "Choose season", c("NE", "SE")),
        sliderInput(inputId = "temp", label = "Temperature", min = 25, max = 31, value = 27, step = .5, ticks = F),
        sliderInput(inputId = "oxy", label = "Oxygen", min = 25, max = 31, value = 27, step = .5, ticks = F),
        sliderInput(inputId = "sal", label = "Salinity", min = 25, max = 31, value = 27, step = .5, ticks = F),
        sliderInput(inputId = "wind", label = "Wind", min = 25, max = 31, value = 27, step = .5, ticks = F),
        sliderInput(inputId = "flo", label = "Fluorescence", min = 25, max = 31, value = 27, step = .5, ticks = F),
        textAreaInput(inputId = "comment", label = "Comment")
          
        )
      
      ),
    card(
      card_header("Michoro", class = "primary", style = "color: steelblue; font-size: 20px; font-weight: bold;"),
      layout_columns(
        col_widths = c(4,4,4),
        plotOutput("bar"),
        plotOutput("hist"),
        plotOutput("box"),
        plotOutput("box2")
        )
      
    )
    
    ),
  layout_columns(
    col_widths = c(6,6), 
    card(
      p("Equibus ciliqua tempor moluptis consequost, vellam estiusanduci dolori quodi sinusda sequis num faccae vollabo rianis evel endi net aut ut vendae ne moluptatum qui aut hari dolorero volupid et et laut quosseq uaspiciam vendent ut dellupt atiorepro ipiet idenimpe con et atur, qui dolorec aepeleniet lit, nosam ne lam vendit apicae sustibea exerum velendipsam ipiet autassin perspelit, qui blabore optaspitem esto cuptatem sitibus viditem ea qui is ut rerspel estionsedi to odionem accaecu sandam erunte eaqui officie ndioribus,")
    ), 
    card()
    )
  )


server = function(input, output, session){
  
  output$bar = renderPlot({
    palmerpenguins::penguins |> 
      group_by(island) |> 
      summarise(n = n()) |> 
      ggplot(aes(x = island, y = n)) +
      geom_col(fill = "steelblue")
  })
  
  output$hist = renderPlot({
    palmerpenguins::penguins |> 
      group_by(island) |> 
      summarise(n = n()) |> 
      ggplot(aes(x = island, y = n)) +
      geom_col()
  })
  
  output$box = renderPlot({
    palmerpenguins::penguins |> 
      group_by(island) |> 
      summarise(n = n()) |> 
      ggplot(aes(x = island, y = n)) +
      geom_col()
  })
  
  output$box2 = renderPlot({
    palmerpenguins::penguins |> 
      group_by(island) |> 
      summarise(n = n()) |> 
      ggplot(aes(x = island, y = n)) +
      geom_col()
  })
  
}


shinyApp(ui = ui, server = server)


