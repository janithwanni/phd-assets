library(tidyverse)
theme_set(theme_minimal())
# set.seed(345)

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
