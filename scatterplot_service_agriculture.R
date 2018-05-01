library(tidyverse)
source("starter_code.R")

# Lowercase everything in Regions 
dat$region <- tolower(dat$region)

# remove all special characters / spaces
dat$region <- str_replace_all(dat$region, "[[:punct:]]", "_")
dat$region <- str_replace_all(dat$region, fixed(" "), "")


shinyApp(
  
  ui = fluidPage(
    titlePanel("Relationship between Service Industry and Agriculture Industry"),
    
    sidebarLayout(
      sidebarPanel(
      
      radioButtons("which_region", label = "Which region?",
                   choices = c("Asia" = asia_ex_neareast_,
                               "Baltics" = baltics,
                               "Commonwealth of Independent States" = c_w_ofind_states,
                               "Eastern Europe" = easterneurope,
                               "Latin America and Caribbean" = latinamer__carib,
                               "Near East" = neareast,
                               "Northern Africa" = northernafrica,
                               "Northern America" = northernamerica,
                               "Oceania" = oceania,
                               "Sub-Saharan Africa" = sub_saharanafrica,
                               "Western Europe" = westerneurope))
      ),
    
    
    mainPanel(
    plotOutput("scatterplot"))
    )
  ),
  
  
  server = function(input, output) {
    output$scatterplot <- renderPlot({
      
      ggplot(countries) + geom_point(aes_string(x = "agriculture", y = "service", color = input$which_region)) + 
        labs(title = "Relationship between Service Industry and Agriculture Industry")
      })
  },
  
  options = list(height = 650)
)

