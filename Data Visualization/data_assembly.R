## Michael Armesto
## IST 719

library(dplyr)
library(lubridate)

## load in excel sheets
sheets_2019 <- list.files(path = 'C:/Users/marmesto/Documents/Cuse/visualization/project/data/2019/')
sheets_2020 <- list.files(path = 'C:/Users/marmesto/Documents/Cuse/visualization/project/data/2020/')

## turn into dataframe
setwd('C:/Users/marmesto/Documents/Cuse/visualization/project/data/2019/')
tables_2019 <- lapply(sheets_2019, function(x) read.csv(x, stringsAsFactors = FALSE))
data_2019 <- bind_rows(tables_2019, .id = 'id')

setwd('C:/Users/marmesto/Documents/Cuse/visualization/project/data/2020/')
tables_2020 <- lapply(sheets_2020, function(x) read.csv(x, stringsAsFactors = FALSE))
data_2020 <- bind_rows(tables_2020, .id = 'id')

rm(tables_2019, tables_2020)

## load station locations dataset
## https://opendata.dc.gov/datasets/capital-bike-share-locations
stations <- read.csv('C:/Users/marmesto/Documents/Cuse/visualization/project/data/Capital_Bike_Share_Locations.csv',
                     stringsAsFactors = FALSE) %>%
  select(c(TERMINAL_NUMBER, LATITUDE, LONGITUDE))

## alter 2019 to match 2020 data
data_2019_2 <- data_2019 %>%
  rename(started_at = Start.date, ended_at = End.date,
         start_station_name = Start.station, end_station_name = End.station,
         start_station_num = Start.station.number, end_station_num = End.station.number,
         member_casual = Member.type, duration = Duration, bike_id = Bike.number) %>%
  left_join(stations, by = c('start_station_num' = 'TERMINAL_NUMBER')) %>%
  left_join(stations, by = c('end_station_num' = 'TERMINAL_NUMBER')) %>%
  rename(start_lat = LATITUDE.x, start_lng = LONGITUDE.x,
         end_lat = LATITUDE.y, end_lng = LONGITUDE.y)

## combine
data <- bind_rows(data_2020, data_2019_2)

## more edits
data <- data %>%
  mutate_at(c('started_at', 'ended_at'), as_datetime) %>%
  mutate(hour = hour(started_at)) %>%
  mutate(wkday = wday(started_at, label = TRUE, abbr = TRUE,
                      week_start = 1)) %>%
  mutate(month = month(started_at)) %>%
  select(c(-duration, -is_equity, -bike_id, -ride_id)) %>%
  mutate(duration = difftime(ended_at, started_at, units = 'mins'))

## save to RDS to load easier later
saveRDS(data, file = 'C:/Users/marmesto/Documents/Cuse/visualization/project/data/alldata.rds')

rm(data, data_2019, data_2019_2, data_2020, stations)