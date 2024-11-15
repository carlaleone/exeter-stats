# Practical Week 8
# Carla Leone 

## Load data and setwd() ----
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/GIS/Week 8/Practical 1")
library(tidyverse)
library(sf)

# datasets
fires<- read_csv("data/VIIRS_fires.csv") %>%
 st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
 st_transform(crs = 32738)

fires
#make fires into a shape file for mapping

forest_cover<- st_read("data/forest_cover_2017.gpkg")
# is a multipolygon
madagascar<- st_read("data/madagascar.gpkg")
madagascar

ranomafama<- st_read("data/ranomafana.shp")
ranomafama

## Start with Map 1: ----
ggplot()+
  geom_sf(data = forest_cover, fill = "forestgreen", linetype = 0) +
  geom_sf(data= fires, colour = "darkorange") +
  geom_sf(data=ranomafama, fill = "transparent", linewidth = 1.2) +
  geom_sf(data = buffer, fill = "transparent", linewidth = 0.7, linetype = 2)

# make a buffer
?st_buffer()
buffer<- st_buffer(ranomafama, dist = 10000, endCapStyle= "FLAT") %>%
  st_transform(crs = 32738)
buffer<- st_cast(buffer, "MULTIPOLYGON")
buffer

# crop fire and forest to the buffer, using intersections
crop_fires<- st_intersection (buffer, fires, sparse = F)
crop_forest<- st_intersection(buffer, forest_cover, sparse = F)

library(colorspace)
# map with cropped ----
ggplot()+
  geom_sf(data = crop_forest, fill = "forestgreen", linetype = 0) +
  geom_sf(data= crop_fires, colour = "darkorange") +
  geom_sf(data=ranomafama, fill = "transparent", linewidth = 1.2) +
  geom_sf(data = buffer, fill = "transparent", linewidth = 0.7, linetype = 2) +
  geom_sf(data = subset(grid, number_of_fires >0), aes(fill = number_of_fires)) +
  scale_fill_continuous_sequential(palette = "RdPu")

geom_sf(data = subset(grid, number_of_fires >0), aes(fill = number_of_fires))
# make with grid 

?st_filter

grid<- st_make_grid(buffer, cellsize = 1000) %>%
 st_intersection(buffer) %>%
  st_as_sf() #into a data table

grid$number_of_fires<- lengths(st_intersects(grid, crop_fires)) # number of fires per cell

?st_filter
