library(tidyverse)
library(plotly)
library(maps)
source("starter_code.R")


#load world data
world <- map_data("world2")
#Delete Antarctica
world <- subset(world,region!="Antarctica")

# Merge population data to world
world$population <- dat$population[match(world$region, dat$country, nomatch=NA)]

# Fix regions with NA population
na_pop <- world %>% filter(is.na(population)) %>% select(region) %>% unique()
(na_pop_countries <- na_pop[, 1])

# Put population into the United States 
world <- world %>% 
  mutate(population = case_when(region == "USA" ~ 298444215,
                                TRUE ~ as.double(population)))

# "Korea. South" (48846823)  and "Korea. North" (23113019) in dat
world <- world %>% 
  mutate(population = case_when(region == "South Korea" ~ 48846823,
                                TRUE ~ as.double(population)),
         population = case_when(region == "North Korea" ~ 23113019,
                                TRUE ~ as.double(population)))

# United Kingdom 60609153
world <- world %>% 
  mutate(population = case_when(region == "UK" ~ 60609153,
                                TRUE ~ as.double(population)))

# Myanmar 
world <- world %>% 
  mutate(population = case_when(region == "Myanmar" ~ 47382633,
                                TRUE ~ as.double(population)))

# South Sudan 41236378
# Rep Congo 3702314
# Demo Congo 62660551
world <- world %>% 
  mutate(population = case_when(region == "South Sudan" ~ 41236378,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Republic of Congo" ~ 3702314,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Democratic Republic of the Congo" ~ 62660551,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Central African Republic" ~ 4303356,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Bosnia and Herzegovina" ~ 3517000,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Kosovo" ~ 1816000,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Palestine" ~ 4550000,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Vatican" ~ 840,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Ivory Coast" ~ 23700000,
                                TRUE ~ as.double(population)),
         population = case_when(region == "Vatican" ~ 840,
                                TRUE ~ as.double(population)))
         
# Plot
# Use ggplot
p <- ggplot(world, aes(x = long, y = lat, text = region)) +
  geom_polygon(aes(group = group, fill = population)) +
  scale_fill_gradient(name="Population (Billions)", low = "yellow", high = "red",
                      labels = c("1.25", "1.00", "0.75", "0.50", "0.25"),
                      breaks = c(1250000000, 1000000000, 750000000, 500000000, 250000000))
ggplotly(p, tooltip=c("fill", "text"))
