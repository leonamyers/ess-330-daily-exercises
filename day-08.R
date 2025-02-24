#Leona Myers
#02-19-2025

library(tidyverse)
library(ggplot2)
library(dplyr)

url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid_data <- read_csv(url)

state_df <- data.frame(
  state = state.name,
  abb = state.abb,
  region = state.region
)

covid_data <- covid_data %>%
  left_join(state_df, by = c("state" = "state"))

covid_region <- covid_data %>%
  group_by(region, date) %>%
  summarise(
    daily_cases = sum(cases, na.rm = TRUE),
    daily_deaths = sum(deaths, na.rm = TRUE)
  ) %>%
  mutate(
    cumulative_cases = cumsum(daily_cases),
    cumulative_deaths = cumsum(daily_deaths)
  ) %>%
  ungroup()
covid_long <- covid_region %>%
  pivot_longer(cols = c(cumulative_cases, cumulative_deaths), 
               names_to = "metric", 
               values_to = "count")

ggplot(covid_long, aes(x = date, y = count, color = metric)) +
  geom_line() +
  facet_wrap(~ region, scales = "free_y") +
  labs(title = "Cumulative COVID-19 Cases & Deaths by USA Region",
       x = "Date", y = "Count",
       color = "Metric") +
  theme_minimal()

covid_national <- covid_data %>%
  group_by(date) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE),
            total_deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(covid_national, aes(x = date, y = total_cases)) +
  geom_line(color = "blue", linewidth = 1) +  # Use `linewidth` instead of `size`
  geom_point(color = "red", size = 1) +  
  labs(title = "Total COVID-19 Cases in the U.S. Over Time",
       x = "Date",
       y = "Total Cases",
       caption = "Data: NY Times COVID-19 Dataset") +
  theme_minimal()

if (!dir.exists("img")) dir.create("img")  # Ensure 'img' directory exists
ggsave("img/total_cases_plot.png", plot = p, width = 8, height = 5, dpi = 300)

