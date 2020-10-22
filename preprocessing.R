df.power <- read.csv("/Users/robi/Desktop/data.csv", header = T, sep = ",", dec = ".", check.names = FALSE)

df.power <- df.power %>%
  mutate(
    Date_Time = as.POSIXct(Date_Time , format = "%Y-%m-%d %H:%M:%S" , tz = "Etc/GMT+12"),
    Day_Type = wday(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
    Day_Type = factor(Day_Type, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    Month = month(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
    Year = year(Date_Time),
    Holiday = if_else(festivo == "S", "Yes", "No")
    ) %>%
  select(-Date, -Time, -min_dec, -festivo) %>%
  select(Date_Time, Year, Month, Day_Type, Holiday, FasciaAEEG, everything())%>%
  filter(Year == "2019")

write.csv(df.power,"data.csv", row.names = FALSE)
