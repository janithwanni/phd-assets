library(kernelshap)
library(bundle)
library(randomForest)
library(tidyverse)

pop_data <- readr::read_csv(here::here("data/sawtooth_data/pop_data.csv"))
train_data <- readr::read_csv(here::here("data/sawtooth_data/train_data.csv"))
test_data <- readr::read_csv(here::here("data/sawtooth_data/test_data.csv"))
rfmodel <- readRDS(here::here("models/sawtooth_data/rfmodel.rds")) |> bundle::unbundle()
set.seed(345)
nnmodel <- readRDS(here::here("models/sawtooth_data/nnmodel.rds")) |> bundle::unbundle()
#' Predict function to be used for the torch models
#' Takes in `luz_fitted` as a model object 
#' data as a data.frame containing two columns
#' Ideally you should set the factor_levels but we use the default
predict_nn <- function(model, data, factor_levels = c("A", "B")) {
  library(torch)
  library(luz)
  data_torch <- torch_tensor(as.matrix(data)) 
  preds <- predict(model, newdata = data_torch)
  factor(factor_levels[as.numeric(preds > 0.5) + 1])
}

shap_vals_train <- kernelshap(
  rfmodel, 
  train_data |> dplyr::select(x, y),
  pop_data,
  pred_fun = \(object, X, ...) {
    predict(object, X, ...) |> as.numeric()
  }
)

shap_nn_train <- kernelshap(
  nnmodel, 
  train_data |> dplyr::select(x, y),
  pop_data,
  pred_fun = \(object, X, ...) {
    predict_nn(model = object, data = X) |> as.numeric()
  }
)

shap_nn_test <- kernelshap(
  nnmodel, 
  test_data |> dplyr::select(x, y),
  pop_data,
  pred_fun = \(object, X, ...) {
    predict_nn(model = object, data = X) |> as.numeric()
  }
)

shap_vals_test <- kernelshap(
  rfmodel, 
  test_data |> dplyr::select(x, y),
  pop_data,
  pred_fun = \(object, X, ...) {
    predict(object, X, ...) |> as.numeric()
  }
)

saveRDS(shap_vals_train, here::here("explainers/sawtooth_data/shap_rfmodel_train_data.rds"))
saveRDS(shap_vals_test, here::here("explainers/sawtooth_data/shap_rfmodel_test_data.rds"))
saveRDS(shap_nn_train, here::here("explainers/sawtooth_data/shap_nnmodel_train_data.rds"))
saveRDS(shap_nn_test, here::here("explainers/sawtooth_data/shap_nnmodel_test_data.rds"))

predictor_rf <- iml::Predictor$new(
  rfmodel,
  pop_data |> dplyr::select(x,y),
  type = "prob"
)


inferTaskFromModel.luz_module_fitted <- function(model) {
  "classification"
}

predictor_nn <- iml::Predictor$new(
  nnmodel,
  pop_data |> dplyr::select(x,y),
  type = "prob",
  predict.function = function(model, newdata, factor_levels = c("A", "B")) {
      data_torch <- torch::torch_tensor(as.matrix(newdata))
      preds <- luz:::predict.luz_module_fitted(model, newdata = data_torch)
      # factor(factor_levels[as.numeric(preds > 0.5) + 1])
      out <- data.frame(V1 = 1 - as.numeric(preds), V2 = as.numeric(preds ))
      colnames(out) <- factor_levels
      return(out)
  }
)

library(furrr)
plan(multisession)

generate_cfact_obj <- function(input_data) {
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(input_data))
    cf_vals <- furrr::future_map(seq_len(nrow(input_data)), \(i) {
      p()
      library(randomForest)
      local_inst <- input_data[i, c("x", "y")]
      target_class <- ifelse(predict(rfmodel, newdata = local_inst) == "A", "B", "A")
    
      nice_cf_gen <- counterfactuals::NICEClassif$new(
        predictor_rf,
        x_nn_correct = FALSE, # we want to find the nearest x_n that the model thinks is right so it doesn't matter whether the model predicted it correctly or not.
        finish_early = FALSE, # don't just get the x_n and end it
        optimization = "proximity",
        distance_function = "gower_c"
      )
      
      nice_cf <- nice_cf_gen$find_counterfactuals(
        x_interest = local_inst,
        desired_class = target_class,
        desired_prob = c(0.8, 1)
      )
      history <- nice_cf_gen$archive |> imap(~ .x |> mutate(iter = .y)) |> list_rbind()
      return(list(
          history = history,
          cfs = rbind(
              nice_cf_gen$x_nn |> mutate(dest = "x_nn"),
              nice_cf$data |> mutate(dest = "cf")
          )
      ))
    })
  })
  return(cf_vals)
}

cf_vals_train <- generate_cfact_obj(train_data)
cf_vals_test <- generate_cfact_obj(test_data)
saveRDS(cf_vals_train, here::here("explainers/sawtooth_data/cfact_rfmodel_train_data.rds"))
saveRDS(cf_vals_test, here::here("explainers/sawtooth_data/cfact_rfmodel_test_data.rds"))

generate_seq_obj <- function(input_data, model, predictor_obj) {
  cf_vals <- map(seq_len(nrow(input_data)), \(i) {
    local_inst <- input_data[i, c("x", "y")]
    preds <- predict_nn(model, input_data |> select(x,y))
    target_class <- ifelse(preds[i] == "A", "B", "A")
  
    nice_cf_gen <- counterfactuals::NICEClassif$new(
      predictor_obj,
      x_nn_correct = FALSE, # we want to find the nearest x_n that the model thinks is right so it doesn't matter whether the model predicted it correctly or not.
      finish_early = FALSE, # don't just get the x_n and end it
      optimization = "proximity",
      distance_function = "gower_c"
    )
    
    nice_cf <- nice_cf_gen$find_counterfactuals(
      x_interest = local_inst,
      desired_class = target_class,
      desired_prob = c(0.8, 1)
    )
    if(!is.null(nice_cf_gen$archive)){
      history <- nice_cf_gen$archive |> 
        imap(~ .x |> mutate(iter = .y)) |> 
        list_rbind()
    } else {
      history <- NULL
    }
    if(!is.null(nice_cf_gen$x_nn)) {
      cfs <- rbind(
        nice_cf_gen$x_nn |> mutate(dest = "x_nn"),
        nice_cf$data |> mutate(dest = "cf")
      )
    } else {
      cfs <- NULL
    }
    return(list(
        history = history,
        cfs = cfs
    ))
  })
  return(cf_vals)
}

cf_nn_train <- generate_seq_obj(train_data, nnmodel, predictor_nn)
cf_nn_test <- generate_seq_obj(test_data, nnmodel, predictor_nn)
saveRDS(cf_nn_train, here::here("explainers/sawtooth_data/cfact_nnmodel_train_data.rds"))
saveRDS(cf_nn_test, here::here("explainers/sawtooth_data/cfact_nnmodel_test_data.rds"))

