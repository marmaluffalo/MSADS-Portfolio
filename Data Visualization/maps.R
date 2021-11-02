## Michael Armesto
## IST 719

library(dplyr)
library(lubridate)
library(tidyr)
library(ggmap)
library(osmdata)

data <- readRDS('C:/Users/marmesto/Documents/Cuse/visualization/project/data/alldata.rds')

## needs more edits
data2 <- data %>%
  mutate(year = year(started_at)) %>%
  mutate(time = hms::hms(second(started_at), minute(started_at), hour(started_at))) %>%
  mutate(time = as.POSIXct(time)) %>%
  mutate(
    member_casual = case_when(
      member_casual == 'casual' ~ 'Casual',
      member_casual == 'member' ~ 'Member',
      TRUE ~ member_casual)) %>%
  mutate(member_year = paste0(year, " ", member_casual, "s")) %>%
  mutate(member_year = fct_relevel(member_year,
                                   "2019 Members", "2020 Members",
                                   "2020 Casuals", "2019 Casuals"))

## reload the station locationss
## https://opendata.dc.gov/datasets/capital-bike-share-locations
stations <- read.csv('Capital_Bike_Share_Locations.csv',
                     stringsAsFactors = FALSE) %>%
  select(c(TERMINAL_NUMBER, LATITUDE, LONGITUDE))

## create dataset of unique routes
## start and end location combos
routes <- data2 %>%
  filter(start_station_name != end_station_name) %>%
  group_by(start_station_name, end_station_name, year,
           start_lat, start_lng, end_lat, end_lng) %>%
  summarise(rides = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  slice_max(rides, n = 100) %>%
  arrange(year, desc(rides))

routes_2020 <- routes %>%
  filter(year == 2020)
  
routes_2019 <- routes %>%
  filter(year == 2019)


###############
## geography ##
###############
library(tigris)
library(sf)

bbx <- getbb('Washington, DC')

## background of DC but overlaps rivers
counties_DC <- counties(state="DC",cb=T,class="sf",)

## load in shapes of water
get_water <- function(county_GEOID){
  area_water("DC", county_GEOID, class = "sf")
}
water <- do.call(rbind, 
                 lapply(counties_DC$COUNTYFP,get_water))

## get difference between counties/water
st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

## subract water from dc background to get nice image
counties_DC <- st_erase(counties_DC,water)

## parks shapefile
## https://opendata.dc.gov/datasets/national-parks/geoservice?geometry=-77.174%2C38.863%2C-76.847%2C38.909
library(geojsonio)
parks.dir <- 'C:/Users/marmesto/Documents/Cuse/visualization/project/data/geo/parks/'
parks <- geojson_read(paste0(parks.dir, 'National_Parks.geojson'),
                      what = 'sp')
parks_df <- fortify(parks)

## arlington shapefile to complete map across river
arlington <- geojson_read(paste0(parks.dir, 'Arlington_County_Boundary_Polygon.geojson'),
                          what= 'sp')
arlington_df <- fortify(arlington)

## arlington parks
arlingtonparks <- geojson_read(paste0(parks.dir, 'Natural_Lands_Polygons.geojson'),
                          what= 'sp')
arlingtonparks_df <- fortify(arlingtonparks)

## metro rail to add lines
## https://opendata.dc.gov/datasets/metro-lines-regional?geometry=-77.707%2C38.771%2C-76.400%2C38.958
metro <- geojson_read(paste0(parks.dir, 'Metro_Lines.geojson'), what = 'sp')
metro_df <- fortify(metro)

## metro bus lines
metrobus <- geojson_read(paste0(parks.dir, 'Metro_Bus_Lines.geojson'), what = 'sp')
metrobus_df <- fortify(metrobus)

#### the actual plot

ggplot() + 
  geom_sf(data = counties_DC, lwd = 0,
          color = 'black', fill = '#003200') +
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE) +
  geom_polygon(data = arlington_df, aes(long, lat, group = group),
               color = 'black', fill = '#003200') +
  geom_polygon(data = arlingtonparks_df, aes(long, lat, group = group),
               color = '#055A5A', fill = '#005400') +
  geom_polygon(data = parks_df, aes(long, lat, group = group),
               colour = '#055A5A', fill = '#005400') +
  geom_path(data = metro_df, aes(long, lat, group = group),
               colour = 'palegoldenrod', linetype = 2, size = 1) +
  geom_path(data = metrobus_df, aes(long, lat, group = group),
            colour = 'grey', linetype = 4, size = 0.5) +
  geom_curve(data = routes_2019, size = 1,
             aes(x = start_lng, y = start_lat,
                 xend = end_lng, yend = end_lat,
                 color = '2019 Route'),
             curvature = -.5) +
  geom_curve(data = routes_2020, size = 1,
             aes(x = start_lng, y = start_lat,
                 xend = end_lng, yend = end_lat,
                 color = '2020 Route'), 
             curvature = -.3) +
  geom_point(data = routes,
             aes(x = end_lng, y = end_lat,
                 color = 'Station', size = rides)) +
  geom_point(data = routes,
             aes(x = start_lng, y = start_lat,
                 color = 'Station', size = rides)) +
  scale_size_continuous(range = c(1, 4)) +
  scale_color_manual(values=c("#EB6B02","#970808",'ivory3')) +
  guides(color = guide_legend(title = 'Item'),
         size = guide_legend(title = 'Number of Rides')) +
  theme_void()

  

