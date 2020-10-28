df.power <- read.csv("/Users/robi/Desktop/BAEDA_data_analytics/data/data.csv", header = T, sep = ",", dec = ".", check.names = FALSE)

# function to automatically find date column
coldate <- sapply(df.power,   function(x) !all(is.na(as.Date(as.character(x),format = "%Y-%m-%d %H:%M:%S"))))

df.power <- df.power %>%
  mutate(
    Date_Time = as.POSIXct(df.power[,coldate] , format = "%Y-%m-%d %H:%M:%S" , tz = "Etc/GMT+12"),
    Day_Type = wday(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
    Day_Type = factor(Day_Type, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    Month = month(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
    Year = year(Date_Time)
  )



#Date      AE Percent 
#TRUE   FALSE   FALSE 



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
  select(Date_Time, Year, Month, Day_Type, Holiday, FasciaAEEG, everything()) %>%
  filter(Year == "2019")

write.csv(df.power,"data.csv", row.names = FALSE)

# plot

dataframe <-  data
varX <- input$variableX
varY <- input$variableY

aes_x <- data$Day_Type
aes_y <- data$Total_Power
arg_stat <- "identity"

plot <- ggplot(data = dataframe, mapping =  aes_string(x = aes_x, y = aes_y, fill = NULL)) + 
  geom_bar(stat = arg_stat) + 
  theme_bw()

plot


# add attributes to dataframe
data <- read.csv("./data/data.csv", header = T, sep = ",", dec = ".", check.names = FALSE)
attributes(data)

attr(data,'units') <- seq(1,length(colnames(data)))

comment(data) <- seq(1,length(colnames(data)))

# summarize

library(dplyr)
data %>%
  group_by(Year) %>% 
  summarise(across(.cols = everything(), mean))

iris %>%
  group_by(Species) %>%
  summarise(across(starts_with("Sepal"), mean))

ddply(data, .(Year,Month), numcolwise(mean, na.rm=TRUE))

x <- c(2,4,3,1,5,7)
y <- c(3,2,6,3,4,6)
group1 <- c("A","A","A","A","B","B")
group2 <- c("X","X","Y","Y","Z","X")

data <- data.frame(group1, group2, x, y)

aggFunction <- function(dataframe, toAverage, toGroup) {
  aggregate(dataframe[, toAverage], dataframe[, toGroup], mean)
}

aggFunction(data, "x", "group1")


# units

library("readxl")
# xls files
my_data <- read_excel("./data/units.xls")
my_data[1,] <-  ""

write.csv(my_data,"./data/units.csv", row.names = FALSE)

