library(readr)
getwd()
joined <- read_csv("/Users/carlaleone/Desktop/Exeter/GIS/Assignment 1/joined_layer_1.csv")
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

#recalculate the carcass density to carcasses/km squared
joined$density<- (joined$n)/100

#ggplot
# first make the lm
lm_road<-lm(density~distance_road, data=joined)

library(tidyr)
library(ggplot2)

# carcass density
plot_roads<- ggplot(data = joined, aes(x = distance_road, y = density)) +
  geom_point(color = "black", alpha = 0.6) +  # Scatter plot
  coord_cartesian(ylim = c(0, NA)) +  # Restrict y-axis to start at 0
  scale_x_continuous(expand = c(0, 0)) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", fullrange = FALSE) +  # Linear regression line
  labs(
    x = "Distance to nearest road (km)",
    y = expression("Elephant poaching density (km "^2*")")) +
  theme_classic()  # Minimal theme for a cleaner look

plot_roads

plot_roads2 <- ggplot(data = joined, aes(x = distance_road, y = density)) +
  geom_point(size = 3,shape = 21, alpha = 1.2, color = "black", fill = "orange") +  # Scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linetype = "dashed") +  # Linear regression line
  labs(
    x = "Distance to nearest road (km)",
    y = expression("Elephant poaching density (km "^2*")")) +
  scale_x_continuous(limits = c(0, 40), expand = c(0, 0)) +  # Remove padding on x-axis, set min at 0
  scale_y_continuous(limits = c(0, 0.045), expand = c(0, 0)) +
  theme_classic()

plot_roads2

?geom_smooth
?geom_pointrange