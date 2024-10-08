library(tidyverse)
theme_set(theme_minimal())
set.seed(345)

p <- function(d) {
  d |> ggplot(aes(x = x,y = y,color = class)) + geom_point() + coord_equal()
}

rotate_data <- function(data, angle) {
  # Convert the angle from degrees to radians
  theta <- angle * pi / 180
  # Create the rotation matrix
  rotation_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
  # Apply the rotation matrix to the dataset
  mat_data <- as.matrix(data |> select(x,y))
  center <- colMeans(mat_data)
  
  centered_data <- apply(mat_data, 1, \(x) x - center) |> t()
  rotated_data <- centered_data %*% rotation_matrix
  rotated_data <- apply(rotated_data, 1, \(x) x + center) |> t()
  rotated_data <- as_tibble(rotated_data, .name_repair = "unique")
  colnames(rotated_data) <- c("x", "y")
  # Return the rotated dataset
  return(rotated_data |> mutate(class = data$class))
}

overlap_data <- function(data, overlap) {
  a_sect <- data |> filter(class == "A") |> mutate(y = y + overlap)
  b_sect <- data |> filter(class == "B") |> mutate(y = y - overlap)
  return(rbind(a_sect, b_sect))
}

contaminate_data <- function(data, contamination) {
  a_sect <- data |> filter(class == "A")
  b_sect <- data |> filter(class == "B")
  a_target_indices <- sample(nrow(a_sect), size = round(nrow(a_sect) * contamination)) 
  b_target_indices <- sample(nrow(b_sect), size = round(nrow(b_sect) * contamination))
  a_sect[a_target_indices, "class"] <- "B"
  b_sect[b_target_indices, "class"] <- "A"
  return(rbind(a_sect, b_sect))
}

brush_border <- function(data, sin_x, brush) {
  rbind(
    data |> filter(round(abs(y - sin_x)) > 0.75),
    data |> filter(round(abs(y - sin_x)) <= 0.75) |> 
      slice_sample(prop = brush)
  )
}

generate_data <- function(rotation, overlap, contamination, num_points, train_split, brush, seed) {
  set.seed(seed)
  x <- runif(num_points, -3, 3)
  y <- runif(num_points, -3, 3)
  sin_x <- sin(3*pi*x)/2
  cl <- ifelse(y < sin_x, "A", "B")
  # browser()
  data <- tibble(x = x,y = y, class = cl) |>
    brush_border(sin_x, brush) |>
    overlap_data(overlap) |>
    contaminate_data(contamination) |>
    rotate_data(rotation)
  data <- data |> filter(x > -1, x < 1,y > -1, y < 1)
  training_indices <- sample(nrow(data), size = round(nrow(data) * train_split))
  training_data <- data[training_indices, ]
  testing_data <- data[-training_indices, ]
  return(list(pop = data, train = training_data,test = testing_data))
}

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
