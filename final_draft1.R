library(shiny)
library(shinydashboard)
library(tidyverse)
library(reshape2)
library(plotly)
library(magrittr)
# Anne's Code##############################################################################
##############################################################################
countries <- read.csv(url("https://raw.githubusercontent.com/gunhojung/315_final_project/master/new%20countries%202.csv"))

# Colleen's 1st manipulation##############################################################################
dt <- read_csv("https://raw.githubusercontent.com/gunhojung/315_final_project/master/countries.csv")

########################################################################################################
# Colleen's 2nd manipulation
gdp <- read_csv("https://raw.githubusercontent.com/gunhojung/315_final_project/master/gdp_current.csv")
gdpT <- melt(gdp)
gdpT$variable <- as.Date(gdpT$variable, format = "%Y")

life <- read_csv("https://raw.githubusercontent.com/gunhojung/315_final_project/master/life_expectancy.csv")
lifeT <- melt(life)
lifeT$variable <- as.Date(lifeT$variable, format = "%Y")
########################################################################################################
# Jason's code
dat <- read_csv(url("https://raw.githubusercontent.com/gunhojung/315_final_project/master/countries_final.csv"))
world <- read_csv(url("https://raw.githubusercontent.com/gunhojung/315_final_project/master/jason_countries.csv"))

########################################################################################################

#Gunho's 
country_df <- read_csv("https://raw.githubusercontent.com/gunhojung/315_final_project/master/new%20countries%202.csv")
country_df <- country_df %>% 
  dplyr::rename(area = `Area..sq..mi..`,
                pop_density = `Pop..Density..per.sq..mi..`,
                coast_area_ratio = `Coastline..coast.area.ratio.`,
                net_migration = `Net.migration`,
                infant_mortality = `Infant.mortality..per.1000.births.`,
                gdp_per_capita = `GDP....per.capita.`,
                literacy = `Literacy....`,
                phones = `Phones..per.1000.`,
                arable = `Arable....`,
                crops = `Crops....`,
                other = `Other....`)
names(country_df) %<>% tolower

cont_cols <- which(names(country_df) %in% 
                     c("unemployment", "net_migration", "pop_density",
                       "gdp_per_capita", "literacy", "agriculture", 
                       "industry", "service"))
country_cont <- country_df[, cont_cols]
# now I need to get rid of NA values
correlation_matrix <- cor(country_cont, use = "pairwise.complete.obs")

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

lo_tri <- correlation_matrix %>% get_lower_tri %>% melt
lo_tri$value <- round(lo_tri$value,2)

############################################################################


ui <- dashboardPage(
  dashboardHeader(title = "315 Interactive Final Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Anne1", tabName = "first", icon = icon("dashboard")),
      menuItem("Anne2", tabName = "second", icon = icon("th")),
      menuItem("Colleen1", tabName = "third", icon = icon("th")),
      menuItem("Colleen2", tabName = "fourth", icon = icon("th")),
      menuItem("Jason1", tabName = "fifth", icon = icon("th")),
      menuItem("Jason2", tabName = "sixth", icon = icon("th")),
      menuItem("Gunho1", tabName = "seventh", icon = icon("th")),
      menuItem("Gunho2", tabName = "eighth", icon = icon("th"))
      
    )),
  dashboardBody(
    tabItems(
      # Boxes need to be put in a row (or column)
      # First Tab
      tabItem(tabName = "first",
              fluidRow(
                box(plotOutput("makeup_plot", height = 650)),
                
                box(
                  title = "Controls",
                  radioButtons("which_variable", label = "Which variable?",
                               choices = c("Average Unemployment Rate (%)" = "avg.region.unemployment",
                                           "Average GDP (USD)" = "avg.region.GDP",
                                           "Average Population Density" = "avg.region.pop.density"))))),
      # Second Tab
      
      tabItem(tabName = "second",
              fluidRow(
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
                                                   "Western Europe" = "WESTERN EUROPE")),
                    
                    sliderInput("bandwidth_adjust", label = "Adjust the Bandwidth",
                                min = 0.1, max = 1, value = 0.8, step = 0.1),
                    
                    checkboxInput("show_rug", label = "Display Rug Plot")))),
      
      # Third Tab
      tabItem(tabName = "third",
              fluidRow(
                box(selectInput("country", 
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
                                                    "Infant mortality (per 1000 births)"))),
                box(plotOutput("country_MDS")))),
      
      # Fourth Tab
      tabItem(tabName = "fourth",
              fluidRow(
                box(selectInput("time_country", label = "Select a country", 
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
                box(plotOutput("time_series")))),
      
      # Fifth Tab
      tabItem(tabName = "fifth",
              fluidRow(
                box(radioButtons("which_region_jason", label = "Which region?",
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
                                             "Western Europe" = "WESTERN EUROPE")),
                    plotOutput("scatterplot")))),
      
      # Sixth Tab
      tabItem(tabName = "sixth",
              fluidRow(
                box(plotlyOutput("plotly"))
              )),
      
      # Seventh Tab
      tabItem(tabName = "seventh",
              fluidRow(
                box(radioButtons("gunho_variable", label = "Economic Sector",
                                 choices = c("Agriculture" = "agriculture * 100",
                                             "Industry" = "industry * 100",
                                             "Service" = "service * 100")),
                    
                    checkboxGroupInput("gunho_region", label = "gunho Regions?",
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
                                                   "Western Europe" = "WESTERN EUROPE")),
                    
                    sliderInput("point_size", label = "Point size:",
                                min = 0.5, max = 3, value = 2, step = 0.5)),
                box(plotOutput("sector_plot"))
              )),
      
      # Eigth Tab
      
      tabItem(tabName = "eighth",
              fluidRow(
                box(plotlyOutput("cormat_plot", height = 700))
              ))
      
    )
  )
) 


server <- function(input, output) {
  
  #First Tab
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
  
  #Second Tab
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
  
  # Third Tab?
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
  
  
  # Fourth Tab? 
  data_2 <- reactive({
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
    ggplot(data = subset(data_2(), Country %in% input$time_country),
           aes(x = variable, y = value, group = Country, color = Country)) +
      geom_line() +
      scale_x_date(limits = c(min(input$year), max(input$year)),
                   date_labels = "%Y") +
      lbls()
    
  })
  
  
  # Fifth Tab
  output$scatterplot <- renderPlot({
    ggplot(subset(dat, Region %in% c(input$which_region_jason))) + 
      geom_point(aes(x = Agriculture, y = Service)) + 
      labs(title = "Relationship between Service Industry and Agriculture Industry")
  })
  
  
  # Sixth Tab
  output$plotly <- renderPlotly({
    jj <- ggplot(world, aes(x = long, y = lat, text = region)) +
      geom_polygon(aes(group = group, fill = population)) +
      scale_fill_gradient(name="Population (Billions)", low = "yellow2", high = "red",
                          labels = c("1.25", "1.00", "0.75", "0.50", "0.25"),
                          breaks = c(1250000000, 1000000000, 750000000, 500000000, 250000000))
    ggplotly(jj, tooltip=c("fill", "text"))
  })
  
  # Seventh Tab
  output$sector_plot <- renderPlot({
    sector_p <- ggplot(subset(country_df, region %in% c(input$gunho_region))) +
      geom_point(aes_string(x = input$gunho_variable, y = "gdp_per_capita"), 
                 size = input$point_size, alpha = 0.5) +
      scale_y_continuous(limits = c(0, 56000)) +
      geom_smooth(aes_string(x = input$gunho_variable, y = "gdp_per_capita"), 
                  method = "lm", na.rm = TRUE) +
      labs(title = "GDP per Capita by Economic Sector",
           x = "Economic Factor",
           y = "GDP per Capita")
    sector_p
  })
  
  # Eigth Tab
  
  output$cormat_plot <- renderPlotly({
    
    cormat_plot <- ggplot(data = lo_tri, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(limits = c(-1,1), 
                           low = "blue", high = "orangered3") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      scale_x_discrete(labels = c("unemployment" = "Unemployment", "net_migration" = "Net Migration",
                                  "pop_density" = "Population Density", "gdp_per_capita" = "GDP per Capita",
                                  "literacy" = "Literacy", "agriculture" = "Agriculture",
                                  "industry" = "Industry", "service" = "Service")) +
      scale_y_discrete(labels = c("unemployment" = "Unemployment", "net_migration" = "Net Migration",
                                  "pop_density" = "Population Density", "gdp_per_capita" = "GDP per Capita",
                                  "literacy" = "Literacy", "agriculture" = "Agriculture",
                                  "industry" = "Industry", "service" = "Service")) +
      labs(title = "Correlation Heat Map of Economic Factors",
           x = "Economic Factor",
           y = "Economic Factor") +
      theme(plot.margin = margin(10, 10, 25, 10))
    ggplotly(cormat_plot)
  })
  
}

shinyApp(ui, server)
