# Week 11 Practical
# Carla Leone
# December 6

## Library loading ----
library(sf) 
library(tidyverse) 
library(terra) 
library(tidyterra) 
library(colorspace)
??colorspace

getwd()
setwd("/Users/carlaleone/Desktop/Exeter/GIS/Week 11/Practical")


# load the maps: ----
cornwall<- st_read("data/cornwall.gpkg")
buffer<- st_read("data/6nm_limit.shp")
bathymetry<- rast("data/cornwall_6NM_gebco_bathy.tif")
seabirds<- st_read("data/SMP.gpkg")
View(guillemot)
guillemot <- seabirds %>%
  filter(Species == "Common.Guillemot") %>%
  filter(Count > 50)

razorbill<- seabirds %>%
  filter(Species == "Razorbill" )%>%
  filter(Count > 50)

towns<- st_read("data/cornwall_towns.gpkg")

View(towns)
?filter()

## build the plot ----
ggplot() +
  geom_spatvector(data=cornwall) +
  geom_spatraster(data=bathymetry, na.translate =F) +
  geom_spatvector(data=buffer, linetype = "dashed") +
  geom_sf(data = razorbill, mapping = aes(size = Count), fill = "lightgreen",shape =21 ) +
  scale_fill_hypso_c(palette ="wiki-2.0_bathy", limits = c(-85,0)) +
  theme_bw(panel.grid.major = element_line(linetype = 'dashed'))

ggplot() +
  geom_spatraster(data=bathymetry, na.translate =F) +
  scale_fill_hypso_c(palette ="wiki-2.0_bathy", limits = c(-85,0)) +
  geom_sf(data=cornwall, fill ="grey70") +
  geom_sf(data = razorbill, mapping = aes(size = Count), fill = "palegreen",shape =21 ) +
  theme_bw()+
  theme(panel.grid.major = element_line(linetype = 'dashed'))
  


