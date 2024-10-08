source(here::here("scripts/sawtooth_data_utils.R"))
data_ls <- generate_data(
  rotation = 45,
  overlap = 0.1,
  contamination = 0.3,
  num_points = 50000,
  train_split = 0.8,
  brush = 0.2,
  seed = 345
)

# x <- runif(50000, -3, 3)
# y <- runif(50000, -3, 3)
# sin_x <- sin(3*pi*x)/2
# cl <- ifelse(y < sin_x, "A", "B")
# # cl <- ifelse(round(abs(x2 - (sin(3*pi*x1)/2))) < 0.75, "A", "B")
# dx <- tibble(x = x, y= y, class = cl)
# p(dx)
# # p(dx)
# p(dx |> filter(round(abs(y - sin_x)) <= 0.75))
# data <- rbind(
#   dx |> filter(round(abs(y - sin_x)) > 0.75),
#   dx |> filter(round(abs(y - sin_x)) <= 0.75) |> slice_sample(prop = 0.2)
# )
# p(dx)
# data <- rotate_data(data |> select(x, y), 45) |> mutate(cl = data$cl) |>
#   filter(x1 > -1, x1 < 1, x2 > -1, x2 < 1) |>
#   select(x = x1, y = x2, class = cl)
# p(data)

write_csv(data_ls$pop, here::here("data/sawtooth_data/pop_data.csv"))

# set.seed(345)

# training_indices <- sample(nrow(data), size = round(nrow(data) * 0.8))
# training_data <- data[training_indices, ]
# testing_data <- data[-training_indices, ]

write_csv(data_ls$train, here::here("data/sawtooth_data/train_data.csv"))
write_csv(data_ls$test, here::here("data/sawtooth_data/test_data.csv"))
