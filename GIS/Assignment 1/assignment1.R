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
summary(lm_road)

library(tidyr)
library(ggplot2)

# carcass density ----
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

# trying distance from roads as a categorical variable ----
joined$distance_bracket <- cut(joined$distance_road,
                               breaks = seq(0, 40, by = 10),  # Breaks from 0 to 40 with 10 km intervals
                               right = FALSE,  # Interval will be [0, 10), [10, 20), etc.
                               labels = c("0-10", "10-20", "20-30", "30-40"))  # Custom labels

joined$distance_bracket<- as.factor(joined$distance_bracket)

lm_distance_road_categorical <- lm(density~distance_bracket, data=joined)
summary(lm_distance_road_categorical)

boxplot(joined$density ~ joined$distance_bracket)

#ggplot the boxplot ----
box_plot <- ggplot(data = joined, aes(x = distance_bracket, y = density)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red", outlier.size = 1.5) +  # Box plot with outlines
  labs(
    x = "Distance from nearest road (km)",
    y = expression("Elephant poaching density (km "^2*")")) +
  theme_classic()

box_plot

#violin plot? ----
violin_plot <- ggplot(data = joined, aes(x = distance_bracket, y = density)) +
  geom_violin(fill = "lightblue", color = "black", alpha = 0.7) +  # Violin plot
  geom_jitter(color = "black", size = 1, alpha = 0.4, width = 0.1) +  # Jitter for individual points
  labs(
    x = "Distance Bracket (km)",
    y = "Elephant Poaching Density",
    title = "Violin Plot of Elephant Poaching Density by Distance Bracket") +
  theme_classic()  # Minimal theme for a cleaner look

# Display the plot
print(violin_plot)


#
# road and river distance together ----
plot_roads_rivers <- ggplot(data = joined, aes(x = distance_road, y = density)) +
  geom_point(aes(color = "Road"), size = 3, shape = 21, fill = "white", alpha = 0.6) +  # Scatter plot for roads
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", fullrange = FALSE, aes(color = "Road")) +  # Linear regression for roads
  geom_smooth(data = joined, aes(x = distance_river, y = density, color = "River"), method = "lm", se = TRUE, linetype = "dashed", fullrange = FALSE) +  # Linear regression for rivers
  labs(
    x = "Distance (km)",
    y = expression("Elephant Poaching Density (km"^2*")"),
    title = "Elephant Poaching Density by Distance to Roads and Rivers") +
  scale_color_manual(name = "Distance", values = c("Road" = "blue", "River" = "green")) +  # Customize colors
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Set x-axis limits
  scale_y_continuous(limits = c(0, 0.045), expand = c(0, 0)) +  # Set y-axis limits
  theme_classic() 
plot_roads_rivers
