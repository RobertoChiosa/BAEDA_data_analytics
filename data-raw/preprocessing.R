## code to prepare `data` dataset goes here

data <- readRDS("./data-raw/data_raw.rda")

summary(data)

usethis::use_data(data, overwrite = TRUE)
