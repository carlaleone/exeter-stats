# Water analysis
library(readr)
getwd()
water <- read_csv("/Users/carlaleone/Desktop/Exeter/GIS/Assignment 1/water_distances.csv")
View(water)

plot(water$density~water$distance_river)
plot(water$density~water$distance_waterhole)

lm