# preprocessing del file data.csv
#df.power <- read.csv("/Users/robi/Desktop/BAEDA_data_analytics/data/run.csv", header = T, sep = ",", dec = ".", check.names = FALSE)
df.power <- read.csv("/Users/robi/Desktop/BAEDA_DASHBOARD_STUDENTS/data/df_cooling_1.csv", header = T, sep = ",", dec = ".", check.names = FALSE)

boxplot(df.power$power_mech_room)

df.power$power_mech_room[ df.power$power_mech_room<0 ] <- NA
df.power$power_mech_room[ df.power$power_mech_room > 2000 ] <- NA

df.power$power_mech_room <- na_interpolation(df.power$power_mech_room, option = "linear")

summary(df.power)

write_rds(df.power,"/Users/robi/Desktop/BAEDA_DASHBOARD_STUDENTS/data/df_cooling_clean.rds")



df.power <- read_rds("/Users/robi/Desktop/BAEDA_DASHBOARD_STUDENTS/data/df_cooling_clean.rds")
dim(df.power)
summary(df.power)

df.power <- 
  
  df.power %>%
  mutate(
    Date_Time = as.POSIXct(date_time , format = "%Y-%m-%d %H:%M:%S" , tz = "Europe/Rome"), # depend on selected timezone
    Week_Day = wday(Date_Time, label = TRUE, week_start = getOption("lubridate.week.start", 1)), # week start on monday
    Month = month(Date_Time, label = TRUE), # ordered factor
    Month_Day = mday(Date_Time), # numeric
    Year = as.ordered(year(Date_Time)), # ordered factor
    Year_Day = mday(Date_Time), # numeric
    Hour = hour(Date_Time), # numeric
    Minute = minute(Date_Time), # numeric
    min_dec = paste(Hour, Minute*100/60, sep = ".") # numeric
  ) %>%
  na.omit(df.power)


dim(na.omit(df.power))

df.power$date_time[is.na(df.power$Date_Time)]

summary(df.power)
# 
# 
# df.power$power_mech_room[df.power$power_mech_room>2000  df.power$power_mech_room<0] <- NA
# ggplot(data =  df.power,
#        mapping =  aes(x =  as.POSIXct(format(ymd_hms(df.power$date_time), "%H:%M:%S"),"%H:%M:%S", tz = "Etc/GMT+12"),
#                       y =  date(df.power$date_time),
#                       fill = df.power$power_mech_room
#        )
# ) +
#   geom_tile() +
#   scale_y_date(
#     breaks = scales::date_breaks("1 month"),                    # specify breaks every two months
#     labels = scales::date_format("%b" , tz = "Etc/GMT+12"),  # specify format of labels anno mese
#     expand = c(0,0)                                     # espande l'asse y affinche riempia tutto il box in verticale
#   ) +
#   scale_x_datetime(
#     breaks = scales::date_breaks("4 hour"),                     # specify breaks every 4 hours
#     labels = scales::date_format(("%H:%M") , tz = "Etc/GMT+12"),# specify format of labels ora minuti
#     expand = c(0,0)                                     # espande l'asse x affinche riempia tutto il box in orizzontale
#   ) +
#   theme_bw()
# 
# 
# 
# skim(df.power)
# summary(df.power)
# 
# t <- sapply(df.power,class)
# dateFormats <- c("%Y-%m-%d %H:%M:%S",
#                  "%m/%d %H:%M:%S",
#                  "%d/%m/%y %H:%M")
# 
# # function to automatically find date column
# coldate <- sapply(df.power,   function(x) !all(is.na(as.Date(as.character(x),format = dateFormats))))
# 
# 
# df.power <- df.power %>%
#   mutate(
#     Date_Time = as.POSIXct(df.power[,coldate] , format = "%Y-%m-%d %H:%M:%S" , tz = "Europe/Rome"),
#     Week_Day = wday(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
#     Week_Day = factor(Week_Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
#     Month = month(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
#     Month_Day = mday(Date_Time),
#     Year = year(Date_Time),
#     Year_Day = mday(Date_Time),
#     Hour = hour(Date_Time),
#     Minute = minute(Date_Time),
#     Holiday = if_else(festivo == "S", "Yes", "No"),
#     Tair = TempARIA,
#     Hglobal = RadGLOBale
#   ) %>%
#   select(-Date, -Time, -Day_Type, -min_dec, -festivo, -TempARIA, -RadGLOBale) %>%
#   select(Date_Time, Year, Year_Day, Month, Month_Day, Week_Day, Hour, Minute, Holiday, everything() ) %>%
#   filter(Year >= 2018)
# 
# 
# 
# 
# 
# #Date      AE Percent
# #TRUE   FALSE   FALSE
# 
# 
# 
# df.power <- df.power %>%
#   mutate(
#     Date_Time = as.POSIXct(Date_Time , format = "%Y-%m-%d %H:%M:%S" , tz = "Etc/GMT+12"),
#     Day_Type = wday(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
#     Day_Type = factor(Day_Type, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
#     Month = month(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
#     Year = year(Date_Time),
#     Holiday = if_else(festivo == "S", "Yes", "No")
#   ) %>%
#   select(-Date, -Time, -min_dec, -festivo) %>%
#   select(Date_Time, Year, Month, Day_Type, Holiday, FasciaAEEG, everything()) %>%
#   filter(Year == "2019")
# 
# write.csv(df.power,"data.csv", row.names = FALSE)
# 
# # plot
# 
# dataframe <-  data
# varX <- input$variableX
# varY <- input$variableY
# 
# aes_x <- data$Day_Type
# aes_y <- data$Total_Power
# arg_stat <- "identity"
# 
# plot <- ggplot(data = dataframe, mapping =  aes_string(x = aes_x, y = aes_y, fill = NULL)) +
#   geom_bar(stat = arg_stat) +
#   theme_bw()
# 
# plot
# 
# 
# # add attributes to dataframe
# data <- read.csv("./data/data.csv", header = T, sep = ",", dec = ".", check.names = FALSE)
# attributes(data)
# 
# attr(data,'units') <- seq(1,length(colnames(data)))
# 
# comment(data) <- seq(1,length(colnames(data)))
# 
# # summarize
# 
# library(dplyr)
# data %>%
#   group_by(Year) %>%
#   summarise(across(.cols = everything(), mean))
# 
# iris %>%
#   group_by(Species) %>%
#   summarise(across(starts_with("Sepal"), mean))
# 
# ddply(data, .(Year,Month), numcolwise(mean, na.rm=TRUE))
# 
# x <- c(2,4,3,1,5,7)
# y <- c(3,2,6,3,4,6)
# group1 <- c("A","A","A","A","B","B")
# group2 <- c("X","X","Y","Y","Z","X")
# 
# data <- data.frame(group1, group2, x, y)
# 
# aggFunction <- function(dataframe, toAverage, toGroup) {
#   aggregate(dataframe[, toAverage], dataframe[, toGroup], mean)
# }
# 
# aggFunction(data, "x", "group1")
# 
# 
# # units
# 
# library("readxl")
# # xls files
# my_data <- read_excel("./data/units.xls")
# my_data[1,] <-  ""
# 
# write.csv(tt,"./data/units.csv", row.names = FALSE)
# 
# # units of measure
# ud_units
# df_units <- data.frame(name = NULL)
# for (i in 1:3248) {
#   df_units[i,1] <- units(tt[[i]])$numerator
# }
# 
# colnames(df_units) <- "Symbol"
# write.csv(df_units,"./data/units.csv", row.names = FALSE)
# 
# 
