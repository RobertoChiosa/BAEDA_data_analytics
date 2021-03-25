## code to prepare `data` dataset goes here

data <- readRDS("./data-raw/data_raw.rda")

data <- readRDS("./data_raw.rda")

save(data, "data.csv")

summary(data)

usethis::use_data(data, overwrite = TRUE)
