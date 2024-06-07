install.packages("maps")
library(tidyverse)
library(maps)
covid_cases_pa <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_cases_deaths.csv")

counties <- map_data("county")
counties_pa <- subset(counties, region == "pennsylvania")

covid_cases_pa <- covid_cases_pa |>
  mutate(county = tolower(county))


covid_cases_avg <- covid_cases_pa |>
  group_by(county) |>
  summarise(avg_cases = mean(cases, na.rm = TRUE))

covid_cases_map <- left_join(counties_pa, covid_cases_avg, by = c("subregion" = "county"))



ggplot(covid_cases_map, aes(x = long, y = lat, group = group, fill = avg_cases)) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(colors = c("yellow", "orange", "red"),
                       guide = guide_colorbar(title.position = "top")) +
  coord_map("polyconic")

