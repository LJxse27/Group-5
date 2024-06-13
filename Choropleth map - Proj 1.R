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


top_5_counties <- covid_cases_avg |>
  arrange(desc(avg_cases)) |>
  slice(1:5)

centroids <- covid_cases_map %>%
  group_by(subregion) %>%
  summarise(long = mean(long), lat = mean(lat))

top_5_centroids <- left_join(top_5_counties, centroids, by = c("county" = "subregion"))

ggplot(covid_cases_map, aes(x = long, y = lat, group = group, fill = avg_cases)) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(colors = c("yellow", "orange", "red"),
                       labels = scales::comma,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map("polyconic") +
  labs(fill = "Average Cases") +
  geom_text(data = top_5_centroids, aes(x = long, y = lat, label = county), color = "black", size = 3, inherit.aes = FALSE)

############################ Bivariate choropleth map ###################################

install.packages("maps")
install.packages("tidyverse")
install.packages("biscale")
install.packages("sf")
install.packages("cowplot")

library(tidyverse)
library(maps)
library(biscale)
library(sf)
library(cowplot)

covid_cases_pa <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_cases_deaths.csv")

# Convert county names to lowercase
covid_cases_pa <- covid_cases_pa |>
  mutate(county = tolower(county))

# Average cases and deaths per county
covid_cases_avg <- covid_cases_pa |>
  group_by(county) |>
  summarise(avg_cases = mean(cases, na.rm = TRUE))

covid_deaths_avg <- covid_cases_pa |>
  group_by(county) |>
  summarise(avg_deaths = mean(deaths, na.rm = TRUE))

# Get county map data
counties <- map_data("county")
counties_pa <- subset(counties, region == "pennsylvania")

# Convert to sf object
counties_pa_sf <- counties_pa |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> #Sets the coordinate reference system (CRS) to EPSG:4326, which is the standard geographic coordinate system (WGS 84).
  group_by(region, subregion, group) |>
  summarize(do_union = FALSE) |> #Ensures that geometries are not dissolved (combined) during the summarization. This is useful for keeping individual geometries intact.
  st_cast("POLYGON") |> #Converts the geometries to POLYGON type. This step ensures that the geometries are in the correct format before converting to MULTIPOLYGON.
  st_cast("MULTIPOLYGON") #This step is necessary because some counties might consist of multiple polygons (e.g., islands or disjoint areas), and converting them to MULTIPOLYGON ensures that all parts of a county are treated as a single unit.

# Merge map data with COVID-19 data
covid_cases_map <- left_join(counties_pa_sf, covid_cases_avg, by = c("subregion" = "county"))
covid_cases_n_deaths <- left_join(covid_cases_map, covid_deaths_avg, by = c("subregion" = "county"))

# Classify the data using bi_class
covid_cases_n_deaths <- covid_cases_n_deaths |>
  bi_class(x = avg_cases, y = avg_deaths, style = "quantile", dim = 3)

# Bivariate map
bivariate_map <- ggplot() +
  geom_sf(data = covid_cases_n_deaths, aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Average COVID-19 Cases and Deaths in PA",
    subtitle = "Bivariate Map"
  ) +
  bi_theme()

# Display the map
print(bivariate_map)

# Add a bivariate legend
legend <- bi_legend(pal = "GrPink", dim = 3, #Specifies the color palette used for the bivariate map. It should match the palette used in bi_scale_fill.
                    xlab = "Average Cases",
                    ylab = "Average Deaths",
                    size = 8)

# Combine map and legend using cowplot
combined_map <- ggdraw() +
  draw_plot(bivariate_map, 0, 0, 1, 1) + # (0,0) The position of the bottom-left corner of the map (relative to the drawing layer). (1, 1) The width and height of the map (relative to the drawing layer).
  draw_plot(legend, 0.6, 0.05, 0.2, 0.2) #(position X, position Y, width, height)

print(combined_map)