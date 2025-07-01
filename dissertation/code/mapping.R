# Mapping
# Carla Leone
# 19 June 2025

### Tring OSM Data ----
library(pacman)
pacman::p_load(osmdata, sf, terra, tidyverse, colorspace, tidyterra)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")

bbox <- as.numeric(st_bbox(c(xmin = -5.159896, xmax = -4.874479, ymax = 50.10781, ymin = 50.23385), crs = st_crs(3857)))
bbox

bbox1<- as.numeric(st_bbox(bathy,crs = st_crs(4326)))
bbox1
query <- opq(bbox)


bathy<- rast("data/Mean depth in multi colour (no land).geotif 2")
plot(bathy)
terra::describe(bathy)
r_depth <- bathy * 0.01666667 
plot(r_depth)

coast <- opq(bbox = bbox) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()
coast <- coast$osm_lines 

bathy_extent <- as.polygons(ext(depth_rast), crs = crs(depth_rast)) |> st_as_sf()
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

### Trying with data in csv format----
getwd()
df <- read_csv("data/Mean depth rainbow colour (no land).csv")
head(df)
df$elevation<- as.numeric(df$elevation)
df<- df %>% filter(!is.na(elevation))


points_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = crs(depth_rast))
# Define raster template (resolution: 0.01 degrees here)
template_rast <- rast(ext(points_sf), resolution = 0.001,crs = crs(depth_rast) )


# Rasterize the depth values
depth_rast <- rasterize(points_sf, template_rast, field = "elevation")
plot(depth_rast)

depth_df <- as.data.frame(depth_rast, xy = TRUE)

ggplot(depth_df, aes(x = x, y = y, fill = last)) +
  geom_tile() +  # Helps reduce grid lines
  scale_fill_viridis_c() +
  theme_minimal()

depth_rast
res(depth_rast)
writeRaster(depth_rast,
            filename = "~/Desktop/depth_raster.tif",
            filetype = "GTiff",      
            overwrite = TRUE)

depth_df <- as.data.frame(depth_rast, xy = TRUE, na.rm = TRUE)
head(depth_df)

hcl_palettes(plot=TRUE)
ggplot() +
  geom_spatraster(data = depth_rast) +
  scale_fill_continuous_sequential("Blues 3", na.value = "transparent", rev= FALSE) 

