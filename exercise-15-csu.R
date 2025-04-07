library(tidymodels)

data("penguins", package = "palmerpenguins")
set.seed(123)
penguins <- na.omit(penguins)
data_split <- initial_split(penguins, prop = 0.7, strata = species)
train_data <- training(data_split)
test_data <- testing(data_split)
cv_folds <- vfold_cv(train_data, v = 10)
cv_folds

# ---Model Fitting and Workflow---

log_reg_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

rand_forest_model <- rand_forest() %>% 
  set_engine("ranger") %>%  
  set_mode("classification")

penguins_recipe <- recipe(species ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes())

log_reg_wf <- workflow() %>%
  add_model(log_reg_model) %>%
  add_recipe(penguins_recipe)

rand_forest_wf <- workflow() %>%
  add_model(rand_forest_model) %>%
  add_recipe(penguins_recipe)


models <- workflow_set(
  preproc = list(penguins_recipe),
  models = list(log_reg = log_reg_model, rand_forest = rand_forest_model)
)


results <- models %>%
  workflow_map("fit_resamples", resamples = cv_folds, metrics = metric_set(accuracy))


ranked_results <- rank_results(results, rank_metric = "accuracy")


print(ranked_results)

## Based on accuracy, the logistic model performs well, but the random forest model improves accuracy by capturing complex relationships
