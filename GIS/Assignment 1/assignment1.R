library(readr)
joined <- read_csv("GIS/Assignment 1/joined_layer_1.csv")
View(joined)
plot(joined$dist_patrol~joined$n)
plot(joined$n~joined$distance_river)
joined$n<- as.numeric(joined$n)

