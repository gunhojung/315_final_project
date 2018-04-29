library(tidyverse)

# Load in dataset
dat <- read_csv("countries.csv")

# Observe data
str(dat)
head(dat)
tail(dat)

# Rename column names in dataset
dat <- dat %>% 
  rename(area = `Area (sq. mi.)`,
         pop_density = `Pop. Density (per sq. mi.)`,
         coast_area_ratio = `Coastline (coast/area ratio)`,
         net_migration = `Net migration`,
         infant_mortality = `Infant mortality (per 1000 births)`,
         gdp_per_capita = `GDP ($ per capita)`,
         literacy = `Literacy (%)`,
         phones = `Phones (per 1000)`,
         arable = `Arable (%)`,
         crops = `Crops (%)`,
         other = `Other (%)`)

# Lowercase column names 
names(dat) %<>% tolower

# Relationship between gdp per capita and mortality rate?
dat %>% 
  ggplot(aes(x = gdp_per_capita, y = infant_mortality)) +
  geom_point() 
# Quick version: qplot(x = gdp_per_capita, y = infant_mortality, data = dat)

# Relationship between crops and agriculture?
qplot(y = crops, x = agriculture, data = dat)


# Relationship between area and arable?
qplot(y = area, x = arable, data = dat)