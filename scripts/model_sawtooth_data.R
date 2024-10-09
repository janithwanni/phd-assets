library(randomForest)
library(tidyverse)
library(bundle)
library(torch)
library(luz)
set.seed(345)

train_data <- readr::read_csv(here::here("data/sawtooth_data/train_data.csv"))
test_data <- readr::read_csv(here::here("data/sawtooth_data/test_data.csv"))
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


# Apparently there is a smarter way of doing this
# I just didn't read the book properly
# train_data_torch_fn <- dataset(
#   name = "sawtooth training data",
#   initialize = function(df) {
#     self$x <- as.matrix(df$x) |> torch_tensor()
#     self$y <- as.matrix(df$y) |> torch_tensor()
#   },
#   .getitem = function(i) list(x = self$x[i, ], y = self$y[i]),
#   .length = function() dim(self$x)[1]
# )
# train_data_torch <- train_data_torch_fn(train_data)

train_data_torch <- tensor_dataset(
  x = torch_tensor(as.matrix(train_data |> select(x,y))),
  y = torch_tensor(
    train_data |> 
      mutate(class = as.numeric(factor(`class`)) - 1) |> 
      pull(class)
  )$to(torch_float()) |>
    torch_reshape(list(-1, 1))
)

test_data_torch <- tensor_dataset(
  x = torch_tensor(as.matrix(test_data |> select(x,y))),
  y = torch_tensor(
    test_data |> 
      mutate(class = as.numeric(factor(`class`)) - 1) |> 
      pull(class)
  )$to(torch_float()) |>
    torch_reshape(list(-1, 1))
)

# Defining a data loader for batches
batch_size <- 32
train_dl <- dataloader(train_data_torch, batch_size = batch_size, shuffle = TRUE)
d_hidden <- 8
d_out <- 1
d_in <- 2

net <- nn_module(
  initialize = function(d_in, d_hidden, d_out) {
    self$net <- nn_sequential(
      nn_linear(d_in, d_hidden),
      nn_relu(),
      nn_linear(d_hidden, d_hidden),
      nn_relu(),
      nn_linear(d_hidden, d_out),
      nn_sigmoid()
    )
  },
  forward = function(x) {
    self$net(x)
  }
)

fitted <- net |> setup(loss = nn_bce_loss(), optimizer = optim_adam) |>
  set_hparams(d_in = d_in, d_hidden = d_hidden, d_out = d_out) |>
  fit(train_dl, epochs = 100)

saveRDS(bundle::bundle(fitted), here::here("models/sawtooth_data/nnmodel.rds"))

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

predict_nn <- function(model, data, factor_levels = c("A", "B")) {
  library(torch)
  library(luz)
  data_torch <- torch_tensor(as.matrix(data)) 
  preds <- predict(model, newdata = data_torch)
  factor(factor_levels[as.numeric(preds > 0.5) + 1])
}

train_data |> 
  mutate(
    class = factor(class),
    preds = predict_nn(fitted, train_data |> select(x,y))
  ) |>
  conf_mat(class, preds)
