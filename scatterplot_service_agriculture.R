library(tidyverse)
library(shiny)
dat <- read.csv(url("https://raw.githubusercontent.com/gunhojung/315_final_project/master/new%20countries%202.csv"))

shinyApp(
  
  ui = fluidPage(
    titlePanel("Relationship between Service Industry and Agriculture Industry"),
    
    sidebarLayout(
      sidebarPanel(
      
      radioButtons("which_region", label = "Which region?",
                   choices = c("Asia" = "ASIA",
                               "Baltics" = "BALTICS",
                               "Commonwealth of Independent States" = "CIS",
                               "Eastern Europe" = "EASTERN EUROPE",
                               "Latin America and Caribbean" = "LATIN AMER. & CARIB",
                               "Near East" = "NEAR EAST",
                               "Northern Africa" = "NORTHERN AFRICA",
                               "Northern America" = "NORTHERN AMERICA",
                               "Oceania" = "OCEANIA",
                               "Sub-Saharan Africa" = "SUB-SAHARAN AFRICA",
                               "Western Europe" = "WESTERN EUROPE"))
      ),
    
    
    mainPanel(
    plotOutput("scatterplot"))
    )
  ),
  
  
  server = function(input, output) {
    output$scatterplot <- renderPlot({
      
      ggplot(subset(dat, Region %in% c(input$which_region))) + geom_point(aes(x = Agriculture, y = Service, color = input$which_region)) + 
        labs(title = "Relationship between Service Industry and Agriculture Industry")
      })
  },
  
  options = list(height = 650)
)