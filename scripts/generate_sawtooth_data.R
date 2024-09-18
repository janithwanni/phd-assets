library(tidyverse)
theme_set(theme_minimal())
set.seed(345)

p <- function(d) {
  d |> ggplot(aes(x = x1,y = x2,color = cl)) + geom_point() + coord_equal()
}

rotate_data <- function(data, angle) {
  # Convert the angle from degrees to radians
  theta <- angle * pi / 180
  
  # Create the rotation matrix
  rotation_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
  
  # Apply the rotation matrix to the dataset
  data <- as.matrix(data)
  center <- colMeans(data)
  centered_data <- apply(data, 2, \(x) x - center)
  rotated_data <- centered_data %*% rotation_matrix
  rotated_data <- apply(rotated_data, 2, \(x) x + center)
  rotated_data <- as_tibble(rotated_data, .name_repair = "unique")
  colnames(rotated_data) <- c("x1", "x2")
  # Return the rotated dataset
  return(rotated_data)
}

x1 <- runif(50000, -3, 3)
x2 <- runif(50000, -3, 3)
sin_x1 <- sin(3*pi*x1)/2
cl <- ifelse(x2 < sin_x1, "A", "B")
# cl <- ifelse(round(abs(x2 - (sin(3*pi*x1)/2))) < 0.75, "A", "B")
dx <- tibble(x1 = x1, x2 = x2, cl)

p(dx)

dx <- rbind(
  dx |> filter(round(abs(x2 - sin_x1)) > 0.75),
  dx |> filter(round(abs(x2 - sin_x1)) <= 0.75) |> slice_sample(prop = 0.1)
)
p(dx)
dx <- rotate_data(dx |> select(x1, x2), 45) |> mutate(cl = dx$cl) |>
  filter(x1 > -1, x1 < 1, x2 > -1, x2 < 1)
p(dx)


