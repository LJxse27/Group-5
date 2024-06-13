library(tidyverse)
covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_cases_deaths.csv")

library(tidyverse)


covid_cases_deaths |> 
  ggplot(aes(x = cases_cume_rate)) +
  geom_histogram()

covid_cases_deaths |> 
  ggplot(aes(x = cases_cume_rate)) +
  geom_histogram()

library(tidyverse)
install.packages("ggbeeswarm")

library(ggbeeswarm)
covid_cases_deaths |> 
  ggplot(aes(x = cases_cume_rate, y = "")) +
  geom_beeswarm(cex = 3, alpha = 0.5)

covid_cases_deaths |> 
  ggplot(aes(x = date, y = cases_rate)) +
  geom_line(color = "darkorange", lineend = "round") +
  labs( title = "Covid 19 Cases Over Time",
        x = "Date",
        y = "Cases Rate") +
  theme_classic()
  
library(tidyverse)
install.packages("GGally")

library(GGally)
covid_cases_deaths |> 
  select(deaths, cases_rate, population) |> 
  ggpairs(aes(alpha = 0.5, width = 1)) 

library(tidyverse)
install.packages("scales")

library(scales)
covid_cases_deaths |>
  group_by(cases_rate, cases_cume_rate) |>
  summarize(
    freq = n(), 
    joint = n() / nrow(covid_cases_deaths)
  ) |> 
  ggplot(aes(x = cases_rate, y = cases_cume_rate)) +
  geom_tile(aes(fill = freq), color = "white") +
  geom_text(aes(label = scales::percent(joint))) +
  scale_fill_gradient2()


covid_cases_deaths |> 
  ggplot(aes(x = date, y = cases_rate)) +
  geom_line(color = "darkred", lineend = "square") +
  labs( title = "Covid 19 Cases Over Time",
        x = "Date",
        y = "Cases Rate") +
  geom_smooth(method = "lm", linewidth = 1) +
  theme_classic() 

covid_cases_deaths |> 
  filter(month_name %in% c("April", "July", "October", "January")) |>
  ggplot(aes(x = covid_cases_deaths)) +
  geom_histogram(bins = 1) +
  facet_grid(covid_cases_deaths ~ ., margins = TRUE)

library(tidyverse) 
install.packages("filtered_data")

filtered_data <- covid_cases_deaths %>%
  filter(month_name %in% c("April", "July", "October", "January"))
filtered_data %>%
  ggplot(aes(x = month_name)) +
  geom_histogram(stat = "count", bins = 1, color = "darkred") +
  facet_grid(. ~ month_name, margins = TRUE) +
  labs(title = "Distribution of COVID Cases and Deaths",
       x = "Month",
       y = "Count") +
  theme_minimal()


library(ggplot2)

library(tidyverse)
covid_cases_deaths <- ("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_cases_deaths.csv")
filtered_data <- covid_cases_deaths %>%
  
ggplot(filtered_data, aes(x = month_name)) +
  geom_histogram(stat = "count", bins = 1, color = "darkred") +
  facet_grid(. ~ month_name, margins = TRUE) +
  labs(title = "Distribution of COVID Cases and Deaths",
       x = "Month",
       y = "Count") +
  theme_minimal()

library(ggplot2)

covid_cases_deaths <- ("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_cases_deaths.csv")

ggplot(covid_cases_deaths %>%
         filter(month_name %in% c("April", "July", "October", "January")), 
       aes(x = month_name)) +
  geom_histogram(stat = "count", bins = 1, color = "darkred") +
  facet_grid(. ~ month_name, margins = TRUE) +
  labs(title = "Distribution of COVID Cases and Deaths",
       x = "Month",
       y = "Count") +
  theme_minimal()

library(tidyverse)
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

library(ggplot2)

covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_cases_deaths.csv")

filtered_data <- covid_cases_deaths %>%
  filter(month_name %in% c("April", "July", "October", "January"))

filtered_data %>%
  ggplot(aes(x = month_name)) +
  geom_histogram(stat = "count", bins = 1, color = "darkred") +
  facet_grid(. ~ month_name, margins = TRUE) +
  labs(title = "Distribution of COVID Cases and Deaths",
       x = "Month",
       y = "Count") +
  theme_minimal()

library(tidyverse)
install.packages("lubridate")

library(lubridate)

# Load required packages
library(lubridate)


# Convert date_column to Date format (if not already)
covid_cases_deaths$date <- as.Date(covid_cases_deaths$date)

# Optionally, you can get month names instead of numeric values
covid_cases_deaths$month_name <- month(covid_cases_deaths$date, label = TRUE)

# Print the updated dataset
print(df)

library(ggplot2)
library(tidyverse)

covid_cases_deaths %>% 
  ggplot(aes(x = date, y = cases_rate)) +
  geom_line(color = "darkred") +
  facet_grid(. ~ month_name, margins = TRUE, scales = "free") +
  labs(title = "Distribution of COVID Cases and Deaths",
       x = "Month",
       y = "Cases Rate") +
  theme_minimal()


