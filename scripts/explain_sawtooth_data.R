library(kernelshap)
library(bundle)
library(randomForest)

pop_data <- readr::read_csv(here::here("data/sawtooth_data/pop_data.csv"))
train_data <- readr::read_csv(here::here("data/sawtooth_data/train_data.csv"))
rfmodel <- readRDS(here::here("models/sawtooth_data/rfmodel.rds")) |> bundle::unbundle()
set.seed(345)

shap_vals <- kernelshap(
  rfmodel, 
  train_data |> dplyr::select(x, y),
  pop_data,
  pred_fun = \(object, X, ...) {
    predict(object, X, ...) |> as.numeric()
  }
)
saveRDS(shap_vals, here::here("explainers/sawtooth_data/shap_rfmodel.rds"))

predictor_rf <- iml::Predictor$new(
  rfmodel,
  pop_data |> dplyr::select(x,y),
  type = "prob"
)

library(furrr)
plan(multisession)

progressr::with_progress({
  p <- progressr::progressor(steps = nrow(train_data))
  cf_vals <- furrr::future_map(seq_len(nrow(train_data)), \(i) {
    p()
    library(randomForest)
    local_inst <- train_data[i, c("x", "y")]
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
    history <- nice_obj$archive |> imap(~ .x |> mutate(iter = .y)) |> list_rbind()
    return(list(
        history = history,
        cfs = rbind(
            nice_obj$x_nn |> mutate(dest = "x_nn"),
            nice$data |> mutate(dest = "x_nn")
        )
    ))
  })
})

saveRDS(cf_vals, here::here("explainers/sawtooth_data/cfact_rfmodel.rds"))

