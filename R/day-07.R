# Name: Leona Myers
# Date: 2025-02-18
# Purpose: Read COVID-19 data from an online source and visualize top 6 states

library(tidyverse)

covid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/latest/owid-covid-latest.csv"
covid_data <- read_csv(covid_url)

state_data <- covid_data %>%
  filter(location %in% state.name)  

top_states <- state_data %>%
  arrange(desc(total_cases)) %>%
  slice(1:6) %>%
  pull(location)  


covid_time_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/full_data.csv"
covid_time_data <- read_csv(covid_time_url)

top_states_data <- covid_time_data %>%
  filter(location %in% top_states)

p1 <- ggplot(top_states_data, aes(x = date, y = total_cases, color = location)) +
  geom_line() +
  facet_wrap(~location, scales = "free_y") +
  labs(title = "Total COVID-19 Cases Over Time in Top 6 States",
       x = "Date",
       y = "Total Cases",
       color = "State") +
  theme_minimal()

ggsave("img/top_6_states_cases.pdf", plot = p1, width = 10, height = 6, dpi = 300)

usa_data <- covid_time_data %>%
  filter(location == "United States")


p2 <- ggplot(usa_data, aes(x = date, y = new_cases)) +
  geom_col(fill = "blue", alpha = 0.7) +
  labs(title = "Daily COVID-19 Cases in the USA",
       x = "Date",
       y = "New Cases") +
  theme_minimal()

ggsave("img/usa_daily_cases.pdf", plot = p2, width = 10, height = 6, dpi = 300)

