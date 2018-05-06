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
                     c("unemployment", "phones", "pop_density",
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

#########################################################################################################

 # Seventh Tab
      tabItem(tabName = "seventh",
              fluidRow(
                box(radioButtons("gunho_variable", label = "Economic Sector",
                                 choices = c("Agriculture" = "agriculture * 100",
                                             "Industry" = "industry * 100",
                                             "Service" = "service * 100")),
                    
                    checkboxGroupInput("gunho_region", label = "Which Regions? (recommend choosing at most 3 regions)",
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
                    width = 4),
                box(plotOutput("sector_plot", height = 600, width = 500))
              )),
      
      # Eigth Tab
      
      tabItem(tabName = "eighth",
              fluidRow(
                box(plotlyOutput("cormat_plot", width = 900, height = 600))
              ))
####################################################################################################

# Seventh Tab
  sector_label <- reactive({
    if (input$gunho_variable == "agriculture * 100") {
      sector_labels <- labs(title = "GDP per Capita by Economic Sector - Agriculture", 
                     x = "Economic Sector (% in Agriculture)", y = "GDP per Capita")
      return(sector_labels)
    }
    
    if (input$gunho_variable == "industry * 100") {
      sector_labels <- labs(title = "GDP per Capita by Economic Sector - Industry", 
                            x = "Economic Sector (% in Industry)", y = "GDP per Capita")
      return(sector_labels)
    }
    
    if (input$gunho_variable == "service * 100") {
      sector_labels <- labs(title = "GDP per Capita by Economic Sector - Service", 
                            x = "Economic Sector (% in Service)", y = "GDP per Capita")
      return(sector_labels)
    }
  })
  output$sector_plot <- renderPlot({
    sector_p <- ggplot(subset(country_df, region %in% c(input$gunho_region))) +
      geom_point(aes_string(x = input$gunho_variable, y = "gdp_per_capita",
                            color = "region"), alpha = 0.5) +
      scale_y_continuous(limits = c(0, 56000)) +
      geom_smooth(aes_string(x = input$gunho_variable, y = "gdp_per_capita",
                             color = "region"), 
                  method = "lm", na.rm = TRUE, se =FALSE) +
      sector_label()
      #labs(title = "GDP per Capita by Economic Sector",
      #     x = "Economic Factor -" && input$gunho_variable,
      #     y = "GDP per Capita")
    sector_p
  })
  
  # Eigth Tab
  
  output$cormat_plot <- renderPlotly({
    
    cormat_plot <- ggplot(data = lo_tri, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(limits = c(-1,1), 
                           low = "yellow2", high = "red") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            plot.margin = margin(10, 10, 35, 10),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      scale_x_discrete(labels = c("unemployment" = "Unemployment", "phones" = "Phone",
                                  "pop_density" = "Population Density", "gdp_per_capita" = "GDP per Capita",
                                  "literacy" = "Literacy", "agriculture" = "Agriculture",
                                  "industry" = "Industry", "service" = "Service")) +
      scale_y_discrete(labels = c("unemployment" = "Unemployment", "phones" = "Phone",
                                  "pop_density" = "Population Density", "gdp_per_capita" = "GDP per Capita",
                                  "literacy" = "Literacy", "agriculture" = "Agriculture",
                                  "industry" = "Industry", "service" = "Service")) +
      labs(title = "Correlation Heat Map of Economic Factors",
           x = "",
           y = "",
           fill = "Correlation")
    ggplotly(cormat_plot)
  })
