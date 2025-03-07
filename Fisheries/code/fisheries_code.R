# Fisheries Management Assessment 2
# Carla Leone
# Feb 27 2025

## Import data and load packages ----
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/Fisheries")
library(tidyverse)
library(readxl)
total_quantity <- read_excel("Fisheries.xlsx", 
                                    sheet = "Total Quantity")
total_value<- read_excel("Fisheries.xlsx", 
                   sheet = "Total Quality")


## Subset to the focal species ----
quantity <- total_quantity %>%
  filter(Species %in% c("Cuttlefish","Lobsters", "Mackerel", "Cod", "Sole")) %>%
  select(-c(2:6))

value <- total_value %>%
  filter(Species %in% c("Cuttlefish","Lobsters", "Mackerel", "Cod", "Sole"))%>%
  select(-c(2:6))

View(quantity)
View(value)



## Initial plots Quantity----
#first change to long format
quantity_long <- quantity %>%
  pivot_longer(cols = -Species, names_to = "Year", values_to = "n")

View(quantity_long)

# Convert Year to numeric (if necessary)
quantity_long$Year <- as.numeric(quantity_long$Year)

# now plot
ggplot(data = quantity_long, aes(x = Year, y = n, color = Species, group = Species)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_colour_manual(values=cbbPalette)+
  labs(x = "Year",
       y = "Quantity ('000 tonnes)",
       color = "Species") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, max(quantity_long$n, na.rm = TRUE), by = 20))+
  scale_x_continuous(breaks = seq(0, max(quantity_long$Year, na.rm = TRUE), by = 1))


## Initial plots value ----
value_long <- value %>%
  pivot_longer(cols = -Species, names_to = "Year", values_to = "n")

View(value_long)

# Convert Year to numeric (if necessary)
value_long$Year <- as.numeric(value_long$Year)

#colour palatte colour blind freindly
cbbPalette <- c("#009E73", "#F0E442",  "#D55E00", "#CC79A7", "#0072B2")

# now plot
ggplot(data = value_long, aes(x = Year, y = n, color = Species, group = Species)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_colour_manual(values=cbbPalette)+
  labs(x = "Year",
       y = "Value (£ million)",
       color = "Species") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, max(value_long$n, na.rm = TRUE), by = 20))+
  scale_x_continuous(breaks = seq(0, max(value_long$Year, na.rm = TRUE), by = 1))
