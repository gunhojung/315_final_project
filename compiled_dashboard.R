library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "315 Interactive Final Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Anne's Plot", tabName = "first", icon = icon("dashboard")),
      menuItem("Colleen's Plot", tabName = "second", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
    # Boxes need to be put in a row (or column)
    tabItem(tabName = "first",
    fluidRow(
      box(plotOutput("makeup_plot")),
      
      box(
        "Box content", br(), "More box content",
        radioButtons("which_variable", label = "Which variable?",
                     choices = c("Average Unemployment Rate (%)" = "avg.region.unemployment",
                                 "Average GDP (USD)" = "avg.region.GDP",
                                 "Average Population Density" = "avg.region.pop.density"))
      ),
      
      box(plotOutput("gdp_density")),
      
      box(checkboxGroupInput("which_region", label = "Which Regions?",
                             choices = c("Asia" = "ASIA",
                                         "Baltics" = "BALTICS",
                                         "Commonwealth of Independent States" = "CIS",
                                         "Eastern Europe" = "EASTERN EUROPE",
                                         "Latin America/Caribbean" = "LATIN AMER. & CARIB",
                                         "Near East" = "NEAR EAST",
                                         "Northern Africa" = "NORTHERN AFRICA",
                                         "Northern America" = "NORTHERN AMERICA",
                                         "Oceania" = "OCEANIA",
                                         "Sub-Saharan Africa" = "SUB-SAHARAN AFRICA",
                                         "Western Europe" = "WESTERN EUROPE"))),
      
      box(sliderInput("bandwidth_adjust", label = "Adjust the Bandwidth",
                      min = 0.1, max = 1, value = 0.8, step = 0.1)),
      
      box(checkboxInput("show_rug", label = "Display Rug Plot"))
        
      )
      
      )
  ),
  
  tabItem(tabName = "second",
          h2("Widgets tab content"))
 )
)

server <- function(input, output) {
  output$makeup_plot <- renderPlot({
    
    p <- ggplot(countries) + geom_bar(aes_string(x = "Region", fill = input$which_variable)) + 
      labs(title = "Count of Countries per World Region",
           fill = "") +
      coord_flip() + 
      scale_fill_gradient(low = "orangered3", high = "gold") +
      theme(axis.title = element_text(size = 14), 
            axis.text = element_text(size = 14), 
            axis.text.y = element_text(hjust = 0, vjust = 1),
            title = element_text(size = 18))
    
    p
    
  })
  
  output$gdp_density <- renderPlot({
    
    pp <- ggplot(subset(countries, Region %in% c(input$which_region))) + geom_density(aes(x = GDP....per.capita., fill = Region, color = Region), alpha = 0.25, adjust = input$bandwidth_adjust) +
      labs(title = "Empirical Density of Global GDPs per capita",
           x = "GDP per Capita (USD)",
           y = "Density")
    
    if(input$show_rug) {
      pp <- pp + geom_rug(aes(x = GDP....per.capita.)) 
    }
    
    pp
    
  })
}

shinyApp(ui, server)

