library(dplyr)
library(lubridate)
library(scales)

## https://www.kaggle.com/marklvl/bike-sharing-dataset?select=hour.csv
bike1 <- read.csv('C:/Users/marmesto/Documents/Datasets/school/hour.csv',
         sep = ",")

## https://www.capitalbikeshare.com/system-data
setwd('C:/Users/marmesto/Documents/Cuse/visualization/project/data/')
bike20171 <- read.csv('2017Q1-capitalbikeshare-tripdata.csv',
                      stringsAsFactors = FALSE)
bike20172 <- read.csv('2017Q2-capitalbikeshare-tripdata.csv',
                      stringsAsFactors = FALSE)
bike20173 <- read.csv('2017Q3-capitalbikeshare-tripdata.csv',
                      stringsAsFactors = FALSE)
bike20174 <- read.csv('2017Q4-capitalbikeshare-tripdata.csv',
                      stringsAsFactors = FALSE)

bike2017_raw <- rbind(bike20171, bike20172, bike20173, bike20174)
write.csv(x = bike2017_raw, file = 'bike2017_raw.csv')

rm(bike20171, bike20172, bike20173, bike20174)

bike2017 <- bike2017_raw %>%
  mutate_at(c('Start.date', 'End.date'), as_datetime) %>%
  mutate(hour = hour(Start.date)) %>%
  mutate(wkday = wday(Start.date, label = TRUE, abbr = TRUE,
                      week_start = 1)) %>%
  mutate(month = month(Start.date)) %>%
  mutate(time = hms::hms(second(Start.date), minute(Start.date), hour(Start.date))) %>%
  mutate(time = as.POSIXct(time))

## pie chart - rides by member type
ggplot(bike2017, aes(x = "", fill = Member.type)) +
  geom_bar(stat = 'count') + #, width = 1, color = 'white') +
  coord_polar('y', start = 0) +
  theme_void() +
  guides(fill = guide_legend(title = 'Member Type'))

## bar chart - ride count by weekday and member type
ggplot(bike2017, aes(x = wkday, fill = Member.type)) +
  geom_bar(position = 'dodge', stat = 'count') +
  labs(x = 'Day of Week', y = 'Number of Rides')

## box plot
box <- ggplot(bike2017, aes(x = wkday, y = Duration/60)) +
  geom_boxplot()

ylim1 = boxplot.stats(bike2017$Duration/60)$stats[c(1,5)]
box <- box + coord_cartesian(ylim = ylim1*1.05) +
  labs(x = 'Day of Week', y = 'Ride Duration (Minutes)')
box

stations <- unique(bike2017$Start.station.number)

## density by hour
ggplot(bike2017) +
  geom_density(aes(x = time, y = ..scaled..), fill = 'red', alpha = 0.6) +
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%H:%M")) +
  labs(x = 'Time of Day', y = 'Density (Scaled)')
