df.power <- read.csv("/Users/robi/Desktop/BAEDA_DASHBOARD_STUDENTS/File GEAE Benchmarking/ViaBazzi_meteo201819_AUMENTOEFFCALDAIA.csv", header = T, sep = ",", dec = ".", check.names = FALSE)


dateFormats <- c("%Y-%m-%d %H:%M:%S", 
                 "%m/%d %H:%M:%S", # energy+
                 "%m/%d  %H:%M:%S",  # energy+ 2 spazi
                 " %m/%d    %H:%M:%S",  # energy+ 2 spazi +1
                 " %Y    %m/%d    %H:%M:%S",  # energy+ 2 spazi +1
                 "%d/%m/%y %H:%M") # accepted date formats

df2018 <- df.power[c(1:17568),]
df2018 <- mutate(df2018, `Date/Time` = paste(2018, `Date/Time`))

df2019 <- df.power[c(17568:dim(df.power)[1]),]
df2019 <- mutate(df2019, `Date/Time` = paste(2019, `Date/Time`))

df <- rbind(df2018, df2019)

coldate <- sapply(df,   function(x) !all(is.na(as.Date(as.character(x), format = dateFormats))))

rrr<- df %>%
  mutate(
    `Date/Time` = as.POSIXct(df[,coldate], format = " %Y    %m/%d    %H:%M:%S" , tz = "Europe/Rome")
  ) 

write.csv(rrr,"./File GEAE Benchmarking/ViaBazzi_meteo201819_AUMENTOEFFCALDAIA.csv", row.names = FALSE)
