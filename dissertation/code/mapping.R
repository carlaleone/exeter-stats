# Mapping
# Carla Leone
# 19 June 2025

### Tring OSM Data ----
library(pacman)


bbox <- as.numeric(st_bbox(c(xmin = -5.2233, xmax = -4.8556, ymax = 50.2340, ymin = 50.0931), crs = st_crs(3857)))
bbox

bbox1<- as.numeric(st_bbox(bathy,crs = st_crs(4326)))
bbox1
query <- opq(bbox1)


bathy<- rast("data/Mean depth in multi colour (no land).geotif 2")
plot(bathy)
terra::describe(bathy)
r_depth <- bathy * 0.01666667 
plot(r_depth)

coast <- opq(bbox = bbox1) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()
coast <- coast$osm_lines 

bathy_extent <- as.polygons(ext(bathy), crs = crs(bathy)) |> st_as_sf()
coast <- st_transform(coast, crs(bathy_extent))
coast_clipped <- st_intersection(coast, bathy_extent)

### bathy df ----
bathy_df <- as.data.frame(bathy, xy = TRUE, na.rm = TRUE)
View(bathy_df)
colnames(bathy_df)[3] <- "depth"
bathy_df$depth<- as.factor(bathy_df$depth)

View(bathy_df)
summarise(bathy_df)

bathy_df<- bathy_df %>% 
mutate(depth = na_if(depth, "0"))

bathy_df$depth<- as.numeric(bathy_df$depth)
bathy_df$depth<- (bathy_df$depth)/10
str(bathy_df$depth)
range(bathy_df$depth, na.rm = TRUE)
summary(bathy_df)
head(bathy_df)
hist(bathy_df$depth, breaks = 10, main = "Depth distribution", xlab = "Depth (m)")


bathy_df$depth <- -bathy_df$depth

### plot----
library(colorspace)
hcl_palettes(plot=TRUE)
ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = depth)) + 
 scale_fill_continuous_sequential("Blues 3", na.value = "transparent",  name = "Depth (m)") +
  geom_sf(data = coast_clipped, color = "black", lwd = 1.0) +
  coord_sf(xlim = c(-5.173958, -4.861458), ylim = c(50.103125, 50.23333), expand = T) +
  theme_classic()

# ai version
ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = depth)) + 
  scale_fill_gradient(
    name = "Depth (m)",
    low = "lightblue",
    high = "darkblue",
    na.value = "transparent"
  ) +
  geom_sf(data = coast_clipped, color = "black", lwd = 1.0) +
  coord_sf(xlim = c(-5.173958, -4.861458), ylim = c(50.103125, 50.23333), expand = TRUE) +
  theme_classic()

### trying built-in r database from NOAA ----
bathy <- getNOAA.bathy(lon1 = -5.2233, lon2 = -4.8556, lat1 = 50.2340, lat2 = 50.0931, resolution = 1)
plot(bathy, image = TRUE)
