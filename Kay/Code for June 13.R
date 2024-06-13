library(tidyverse)

library(ggplot2)
library(tidyverse)
covid_cases_deaths %>% 
  ggplot(aes(x = date, y = cases_rate)) +
  geom_line(color = "darkblue") +
  facet_grid(. ~ month_name, margins = TRUE, scales = "free") +
  labs(title = "Distribution of COVID Cases and Deaths",
       x = "Month",
       y = "Cases Rate") +
  theme_classic() +
  theme(axis.text.x = element_blank())