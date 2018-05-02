library(tidyverse)
library(shiny)
library(reshape2)

dt <- read_csv("https://raw.githubusercontent.com/gunhojung/315_final_project/master/countries.csv")

# MDS plot of countries 
shinyApp(
  
  ui = fluidPage(
    inputPanel(
      selectInput("country", 
                  label = "Select a country", 
                  choices = dt["Country"], 
                  selected = "United States",
                  multiple = TRUE),
      
      checkboxGroupInput("variable",
                         label = "Select a variable",
                         choices = c("Service", "Agriculture", "Industry",
                                     "Deathrate", "Birthrate", "Unemployment",
                                     "Infant mortality (per 1000 births)", 
                                     "GDP ($ per capita)", "Literacy (%)",
                                     "Phones (per 1000)"),
                         selected = c("GDP ($ per capita)", "Literacy (%)", 
                                      "Phones (per 1000)", 
                                      "Infant mortality (per 1000 births)"))
    ),
    
    plotOutput("country_MDS")
  ),
  
  server = function(input, output, session) {
    data <- reactive({
      country_cont <- dplyr::select(dt, c(input$variable, "Country"))
      country_cont <- na.omit(country_cont)
      country_cont_scale <- scale(country_cont[1:length(country_cont)-1])
      dist_country <- dist(country_cont_scale)
      country_mds <- cmdscale(dist_country, k = 2)
      colnames(country_mds) <- c("mds_coordinate_1", "mds_coordinate_2")
      country_mds <- cbind(country_mds, country_cont["Country"])
      dt <- left_join(dt, country_mds, by = "Country")
    })
    
    output$country_MDS <- renderPlot({
      ggplot(subset(data(), Country %in% input$country), 
             aes(x = mds_coordinate_1, y = mds_coordinate_2, color = Region)) +
        geom_text(aes(label = subset(dt, Country %in% input$country)$Country)) +
        labs(title = "MDS of economic strength measures on countries, colored by region",
             x = "MDS coordinate 1",
             y = "MDS coordinate 2")
    })
  },
  
  options = list(height = 550)
)

gdp <- read_csv("https://raw.githubusercontent.com/gunhojung/315_final_project/master/gdp_current.csv")
gdpT <- melt(gdp)
gdpT$variable <- as.Date(gdpT$variable, format = "%Y")

life <- read_csv("https://raw.githubusercontent.com/gunhojung/315_final_project/master/life_expectancy.csv")
lifeT <- melt(life)
lifeT$variable <- as.Date(lifeT$variable, format = "%Y")

# time sries plot of countries 
shinyApp(
  
  ui = fluidPage(
    inputPanel(
      selectInput("country", label = "Select a country", 
                  choices = dt["Country"], 
                  selected = "United States",
                  multiple = TRUE),
      
      sliderInput("year", label = "Adjust the years",
                  min = as.Date("1960", format = "%Y"),
                  max = as.Date("2016", format = "%Y"),
                  value = c(as.Date("1960", format = "%Y"),
                            as.Date("2016", format = "%Y")),
                  timeFormat = "%Y"),
      
      radioButtons("type", label = "Variable",
                   choices = c("GDP (current US$)", "Life expectancy"),
                   selected = "GDP (current US$)")
    ),
    
    plotOutput("time_series")
  ),
  
  server = function(input, output) {
    data <- reactive({
      if (input$type == "GDP (current US$)") {
        return(gdpT)
      }
      
      if (input$type == "Life expectancy") {
        return(lifeT)
      }
    })
    
    lbls <- reactive({
      if (input$type == "GDP (current US$)") {
        labels <- labs(title = "Time series of GDP (in current US$) by country", 
                       x = "Year", y = "GDP (current US$)")
        return(labels)
      }
      
      if (input$type == "Life expectancy") {
        labels <- labs(title = "Time series of life expectancy (in years) by country",
                       x = "Year", y = "Life expectancy")
      }
    })
    
    output$time_series <- renderPlot({
      ggplot(data = subset(data(), Country %in% input$country),
             aes(x = variable, y = value, group = Country, color = Country)) +
        geom_line() +
        scale_x_date(limits = c(min(input$year), max(input$year)),
                     date_labels = "%Y") +
        lbls()
      })
    },
  
  options = list(height = 550)
)