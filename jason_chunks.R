# Fifth Tab
output$scatterplot <- renderPlot({
  ggplot(subset(dat, Region %in% c(input$which_region_jason)),
         aes(x = Agriculture, y = Service, color = Region)) + 
    geom_smooth(aes(x = Agriculture, y = Service), 
                method = "lm", na.rm = TRUE, se = FALSE) + 
    labs(title = "Relationship between Service Industry and Agriculture Industry")
})


# Sixth Tab
output$plotly <- renderPlotly({
  jj <- ggplot(world, aes(x = long, y = lat, text = region)) +
    geom_polygon(aes(group = group, fill = population)) +
    scale_fill_gradient(name="Population (Billions)", low = "yellow2", high = "red",
                        labels = c("1.25", "1.00", "0.75", "0.50", "0.25"),
                        breaks = c(1250000000, 1000000000, 750000000, 500000000, 250000000)) +
    labs(x = "Longitude(°)", y = "Latitude(°)") +
    ggtitle("Distribution of Population by Countries")
  ggplotly(jj, tooltip=c("fill", "text"))
})




# Fifth Tab
tabItem(tabName = "fifth",
        fluidRow(
          box(checkboxGroupInput("which_region_jason", label = "Which region?",
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
                                             "Western Europe" = "WESTERN EUROPE"))),
          box(plotOutput("scatterplot")))),

# Sixth Tab
tabItem(tabName = "sixth",
        fluidRow(
          box(plotlyOutput("plotly",width = 1100, height = 600))
        ))