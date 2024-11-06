# Water analysis
library(readr)
getwd()
water <- read_csv("/Users/carlaleone/Desktop/Exeter/GIS/Assignment 1/extra_water.csv")
View(water)

hist(water$density)
# water presence in grid cells ----
water$waterhole_presence <- ifelse(!is.na(water$density_waterholes) & water$density_waterholes > 0, "Yes", "No")
water$presence_type <- ifelse(water$river_presence == "Yes" & water$waterhole_presence == "Yes", "both",
                              ifelse(water$river_presence == "No" & water$waterhole_presence == "No", "neither",
                           ifelse(water$river_presence == "Yes", "river",
                                  ifelse(water$waterhole_presence == "Yes", "watering hole", NA))))


#initial plots ----
plot(water$density~water$distance_river)
plot(water$density~water$density_waterholes)
plot(water$density~water$distance_waterhole)
abline(lm_waterhole_distance)

boxplot(water$density~water$presence_type)

#linear models ----
lm_waterhole_density<- lm(density~density_waterholes, data=water)
lm_waterhole_distance<- lm(density~distance_waterhole, data= water)
lm_riverdist<- glm(density~distance_river, family = poisson, data =water,)
combined_lm<- lm(density ~ distance_river*distance_waterhole, data=water)
summary(combined_lm)
summary(lm_riverdist)

# ggplot waterhole density ----
library(ggplot2)
plot_riverdist <- ggplot(data = water, aes(x = distance_river, y = density)) +
  geom_point(size = 3,shape = 21, alpha = 1.2, color = "black", fill = "orange") +  # Scatter plot
  geom_smooth(method = "glm", method.args = list(family = poisson), se= FALSE) +  # Linear regression line
  labs(
    x = "Distance to nearest river (km)",
    y = "Elephant poaching density (carcasses/km²)") +
  scale_x_continuous(limits = c(0, 40), expand = c(0, 0)) +  # Remove padding on x-axis, set min at 0
  scale_y_continuous(limits = c(0,0.045), expand = c(0, 0)) +
  theme_classic()

plot_riverdist


#boxplot ----
boxplot_water<- ggplot(data = water, aes(x = presence_type, y = density, fill = presence_type)) +
  geom_boxplot(color = "darkblue") +
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") + 
  labs(
    x = "Presence of rivers and watering holes",
    y = "Elephant poaching density (carcasses/km²)"
     ) +
  scale_x_discrete( labels = c("river" = "River", "watering hole" = "Watering Hole", "both" = "River + Watering hole", "neither" = "No water")
    ) +
  scale_fill_manual(values = c("river" = "darkblue", "watering hole" = "lightgreen", "both" = "skyblue", "neither" = "orange")) +  # Custom colors
  theme_classic() +
  theme(legend.position = "none")

boxplot_water


# violin plot ----
ggplot(data = water, aes(x = presence_type, y = density, fill = presence_type)) +
  geom_violin(color = "darkblue", alpha = 0.6) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.8) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Raw data points
  labs(
    x = "Presence of rivers and watering holes",
    y = "Elephant poaching density (carcasses/km²)"
  ) +
  scale_x_discrete( labels = c("river" = "River", "watering hole" = "Watering Hole", "both" = "River + Watering hole", "neither" = "No water")
  ) +
  scale_fill_manual(values = c("river" = "darkblue", "watering hole" = "lightgreen", "both" = "skyblue", "neither" = "orange")) +  # Custom colors
  
  theme_classic()+
  theme(legend.position = "none")
