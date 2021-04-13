## code to prepare `data` dataset goes here

data <- readRDS("./data-raw/data_raw.rda")

data <- readRDS("./data_raw.rda")

colnames(data)[1] <- "Date_Time"

save(data, "data.csv")

summary(data)

usethis::use_data(data, overwrite = TRUE)
