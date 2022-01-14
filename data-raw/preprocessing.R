## code to prepare `data` dataset goes here
library(magrittr)

# load data raw
data <-
  read.csv(
    file.path("data-raw", "data.csv"),
    sep = ";",
    dec = ",",
    stringsAsFactors = T
  ) %>%
  dplyr::mutate(
    DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M", tz = "Europe/Rome"),
    Month = lubridate::month(DateTime, label = TRUE, abbr = TRUE),
    #Hour = lubridate::hour(DateTime),
    Weekday = lubridate::wday(
      x = DateTime,
      label = TRUE,
      abbr = TRUE,
      week_start =  getOption("lubridate.week.start", 1)
    )
  ) %>%
  dplyr::filter(DateTime >= "2015-04-01" & DateTime < "2015-08-01") %>% 
  dplyr::rename(
    Date_Time = DateTime,
    ToU  = FasciaAEEG,
    Total_Power = TotalP,
    Chiller = ChillerP,
    Hglobal = Rad,
    Text = Test
  ) %>%
  dplyr::select(Date_Time, Weekday, Month, ToU, dplyr::everything())


# save the data and compress to solve the warning:
# 1) Note: significantly better compression could be obtained by using R CMD build --resave-data
# https://stackoverflow.com/questions/10233593/how-to-effectively-deal-with-uncompressed-saves-during-package-check
# 2) WARNING: Added dependency on R >= 3.5.0 because serialized objects in
# https://stackoverflow.com/questions/63271511/why-are-data-in-my-package-serialized-and-not-being-read-in-the-most-recent-ve
save(
  data,
  file = file.path("data", "data.RData"),
  compress = 'xz',
  version = 2
)

usethis::use_data(data, overwrite = TRUE)
