## code to prepare `data` dataset goes here

# load data raw
data <-
  read.csv(
    file.path("data-raw", "data.csv"),
    sep = ";",
    dec = ",",
    stringsAsFactors = T
  )

# convert to posixct
data$DateTime <-
  as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M", tz = "Europe/Rome")

# change column name
colnames(data)[1] <- "Date_Time"

save(data, file = file.path("data-raw", "data.RData"))

usethis::use_data(data, overwrite = TRUE)
