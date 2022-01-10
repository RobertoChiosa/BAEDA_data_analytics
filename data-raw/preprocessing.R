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
colnames(data) <- c("Date_Time", "ToU", "Total_Power", "Chiller", "Hglobal", "Text")

# save the data and compress to solve the warning:
# 1) Note: significantly better compression could be obtained by using R CMD build --resave-data
# https://stackoverflow.com/questions/10233593/how-to-effectively-deal-with-uncompressed-saves-during-package-check
# 2) WARNING: Added dependency on R >= 3.5.0 because serialized objects in
# https://stackoverflow.com/questions/63271511/why-are-data-in-my-package-serialized-and-not-being-read-in-the-most-recent-ve 
save(data, file = file.path("data", "data.RData"), compress='xz', version = 2)

usethis::use_data(data, overwrite = TRUE)
