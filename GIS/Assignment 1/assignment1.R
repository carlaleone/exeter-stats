library(readr)
joined <- read_csv("GIS/Assignment 1/joined_layer_1.csv")
View(joined)
plot(joined$n~joined$dist_patrol)
plot(joined$n~joined$distance_road)
joined$n<- as.numeric(joined$n)

lm_road<-lm(n~distance_road, data=joined)
summary(lm_road)
plot(lm_road)



plot(joined$n~joined$distance_road)
abline(lm_road)


joined$n<- as.numeric(joined$n)

