#### plot_region and key road 
rm(list = ls());gc()
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(sf)
library(geojson)
library(geojsonio)
library(ggplot2)


shanghai_bound_test <- st_read("shanghai.geojson")
shanghai_bound_test <- shanghai_bound_test[, c("name", "geometry")]
plot(shanghai_bound_test['name'])


###### geojson data from openstreet, export use qgis
## road
shanghai_road1 <- st_read("shanghai_road1.geojson", stringsAsFactors = F)
names(shanghai_road1)


shanghai_road_highway <- shanghai_road1[!is.na(shanghai_road1$highway),]
table(shanghai_road_highway$highway)
# shanghai_road_highway <- shanghai_road_highway[!is.na(shanghai_road_highway$name),]
## filter important road 
shanghai_road_highway <- shanghai_road_highway[shanghai_road_highway$highway %in% c("motorway", "trunk","primary", "secondary"),]
shanghai_road_highway <- select(shanghai_road_highway, name, geometry)

shanghai_road_name <- shanghai_road_highway[!is.na(shanghai_road_highway$name),]


county = c("静安区", "黄浦区")
region_boundry <- shanghai_bound_test[shanghai_bound_test$name %in% county,]
road_in <- st_intersection(region_boundry, shanghai_road_highway)
road_in_text <- group_by(road_in, name.1) %>% slice(1)
road_in_text <- road_in_text[!is.na(road_in_text$name.1),]

aa <- ggplot(region_boundry) +
  geom_sf(color = "black", show.legend = F) + 
  geom_sf(data = road_in, col = "orange", size = 1) + 
  geom_sf_text(aes(label = name.1), data = road_in_text, col = "black", size = 2) + 
  coord_sf(xlim, ylim) +
  ggtitle(paste(county, collapse = "+")) + theme_minimal()
