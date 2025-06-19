# Mapping
# Carla Leone
# 19 June 2025

### Tring OSM Data ----
install.packages("osmdata")
library(osmdata)
library(sf)
?st_bbox

bbox <- as.numeric(st_bbox(c(xmin = -5.2233, xmax = -4.8556, ymax = 50.2340, ymin = 50.0931), crs = st_crs(3857)))
bbox
query <- opq(bbox)
