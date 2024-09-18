library(randomForest)
library(tidyverse)
library(bundle)
set.seed(345)

train_data <- readr::read_csv(here::here("data/sawtooth_data/train_data.csv"))
rfmodel <- randomForest(class ~ x + y, data = train_data |> mutate(class = factor(class)))
rfmodel_bundled <- bundle(rfmodel)
saveRDS(rfmodel_bundled, here::here("models/sawtooth_data/rfmodel.rds"))

library(tidymodels)

xgbmodel <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |> 
  fit(class ~ x + y, data = train_data |> mutate(class = factor(class)))

xgb_bundled <- bundle(xgbmodel)
saveRDS(xgb_bundled, here::here("models/sawtooth_data/xgbmodel.rds"))

train_data |> 
  mutate(
    class = factor(class), 
    preds = predict(xgbmodel, new_data = train_data) |> pull(.pred_class)
  ) |>
  conf_mat(class, preds)

train_data |> 
  mutate(
    class = factor(class), 
    preds = predict(rfmodel, new_data = train_data)
  ) |>
  conf_mat(class, preds)
