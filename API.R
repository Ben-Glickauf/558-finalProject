# Load packages -----------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(plumber)
library(yardstick)


# Read data and fit final model --------------------------------------------

water_data <- read_csv("water_potability.csv") |>
  mutate(Potability = as.factor(Potability))


# Recipe using all predictors
water_recipe <-
  recipe(Potability ~ ., data = water_data) |>
  step_impute_median(all_predictors())


# Final random forest specification
rf_spec <-
  rand_forest(
    trees = 500,
    mtry = 7
  ) |>
  set_engine("ranger") |>
  set_mode("classification")


# Workflow
rf_workflow <-
  workflow() |>
  add_recipe(water_recipe) |>
  add_model(rf_spec)


# Fit model to entire dataset
final_model <-
  fit(
    rf_workflow,
    data = water_data
  )


# Store predictions for confusion matrix -------------------------------

water_predictions <-
  predict(
    final_model,
    new_data = water_data,
    type = "class"
  ) |>
  bind_cols(water_data)


# Pred endpoint -----------------------------------------------------------

#* Predict water potability
#*
#* @serializer json
#* @post /pred
function(
    ph = mean(water_data$ph, na.rm = TRUE),
    Hardness = mean(water_data$Hardness, na.rm = TRUE),
    Solids = mean(water_data$Solids, na.rm = TRUE),
    Chloramines = mean(water_data$Chloramines, na.rm = TRUE),
    Sulfate = mean(water_data$Sulfate, na.rm = TRUE),
    Conductivity = mean(water_data$Conductivity, na.rm = TRUE),
    Organic_carbon = mean(water_data$Organic_carbon, na.rm = TRUE),
    Trihalomethanes = mean(water_data$Trihalomethanes, na.rm = TRUE),
    Turbidity = mean(water_data$Turbidity, na.rm = TRUE)
){
  
  new_water <- tibble(
    ph = as.numeric(ph),
    Hardness = as.numeric(Hardness),
    Solids = as.numeric(Solids),
    Chloramines = as.numeric(Chloramines),
    Sulfate = as.numeric(Sulfate),
    Conductivity = as.numeric(Conductivity),
    Organic_carbon = as.numeric(Organic_carbon),
    Trihalomethanes = as.numeric(Trihalomethanes),
    Turbidity = as.numeric(Turbidity)
  )
  
  
  predict(
    final_model,
    new_data = new_water,
    type = "prob"
  )
}


# Example API calls:
# POST http://127.0.0.1:8000/pred
#
# POST http://127.0.0.1:8000/pred?ph=7&Hardness=200&Solids=15000&Chloramines=7&Sulfate=300&Conductivity=400&Organic_carbon=10&Trihalomethanes=60&Turbidity=4
#
# POST http://127.0.0.1:8000/pred?ph=8&Hardness=250&Solids=10000&Chloramines=6&Sulfate=350&Conductivity=450&Organic_carbon=12&Trihalomethanes=50&Turbidity=3


# Info endpoint -----------------------------------------------------------

#* Provide information about the project
#*
#* @serializer json
#* @get /info
function(){
  
  list(
    name = "Benjamin Glickauf",
    github_pages = "YOUR_GITHUB_PAGES_URL_HERE"
  )
  
}


# Confusion matrix endpoint -----------------------------------------------

#* Plot confusion matrix
#*
#* @png
#* @get /confusion
function(){
  conf_mat(
    data = water_predictions,
    truth = Potability,
    estimate = .pred_class
  ) |>
    autoplot()
  
}
