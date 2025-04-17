
library(tidyverse)
library(tidymodels)
library(lubridate)
library(janitor)

set.seed(123)


covid_url <-  'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
pop_url   <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv'


covid_data   <- readr::read_csv(covid_url)
census_data <- co_est2023_alldata


latest_covid <- covid_data %>%
  filter(date == max(date)) %>%
  select(state, deaths) %>%
  group_by(state) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE))


census_clean <- census_data %>%
  janitor::clean_names() %>%       
  filter(sumlev == 40) %>%         
  transmute(
    state = stname,                
    population_2023 = popestimate2023,
    population_2020 = popestimate2020,
    change = popestimate2023 - popestimate2020,
    percent_change = (popestimate2023 - popestimate2020) / popestimate2020 * 100
  )



df <- left_join(latest_covid, census_clean, by = "state") %>%
  drop_na() %>%
  mutate(
    log_deaths = log1p(deaths),
    log_pop = log1p(population_2023),
    log_change = log1p(change)
  )


set.seed(42)
data_split <- initial_split(df, prop = 0.8)
train_data <- training(data_split)
test_data  <- testing(data_split)


death_recipe <- recipe(log_deaths ~ log_pop + percent_change + log_change, data = train_data) %>%
  step_normalize(all_predictors())


rf_model <- rand_forest(trees = 500) %>%
  set_mode("regression") %>%
  set_engine("ranger")


wf <- workflow() %>%
  add_recipe(death_recipe) %>%
  add_model(rf_model)


rf_fit <- fit(wf, data = train_data)


predictions <- predict(rf_fit, test_data) %>%
  bind_cols(test_data) %>%
  mutate(predicted_deaths = expm1(.pred), actual_deaths = expm1(log_deaths))


metrics <- predictions %>%
  metrics(truth = actual_deaths, estimate = predicted_deaths)

print(metrics)


ggplot(predictions, aes(x = actual_deaths, y = predicted_deaths)) +
  geom_point(color = "dodgerblue", size = 3) +
  geom_abline(linetype = "dashed", color = "darkred") +
  labs(
    title = "Predicted vs Actual COVID-19 Deaths by State",
    x = "Actual Deaths",
    y = "Predicted Deaths"
  ) +
  theme_minimal()
