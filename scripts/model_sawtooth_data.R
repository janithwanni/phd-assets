library(randomForest)
library(bundle)
set.seed(345)

train_data <- readr::read_csv(here::here("data/sawtooth_data/train_data.csv"))
rfmodel <- randomForest(class ~ x + y, data = train_data |> mutate(class = factor(class)))
rfmodel_bundled <- bundle(rfmodel)
saveRDS(rfmodel_bundled, here::here("data/sawtooth_data/rfmodel.rds"))