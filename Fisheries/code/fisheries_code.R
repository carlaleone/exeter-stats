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
                   sheet = "Total Value")
scotland <- read_excel("Fisheries.xlsx", 
                       sheet = "scottish fleet")

value_and_catch_total <- read_excel("Fisheries.xlsx", 
                       sheet = "value and catch total uk") 

## Subset to the focal species ----
quantity <- total_quantity %>%
  filter(Species %in% c("Cuttlefish","Lobsters", "Mackerel", "Cod", "Sole", "Total All Species")) %>%
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

## Total catch and value UK ----
value_and_catch_total_long <- value_and_catch_total %>%
  pivot_longer(cols = -n, names_to = "Year", values_to = "Value") %>%
  rename(type = n)  %>%
  mutate(type = case_when(
    type == "value" ~ "Value (million £)",
    type == "quantity" ~ "Catch (tonnes)"
  ))

value_and_catch_total_long$Year<- as.numeric(value_and_catch_total_long$Year)

# now plot
ggplot(data = value_and_catch_total_long, aes(x = Year, y = Value, color = type, group = type)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_colour_manual(values=cbbPalette)+
  labs(x = "Year",
       y = "Quantity ('000 tonnes)",
       color = "Species") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, max(quantity_long$n, na.rm = TRUE), by = 20))+
  scale_x_continuous(breaks = seq(0, max(quantity_long$Year, na.rm = TRUE), by = 1))


ggplot(value_and_catch_total_long, aes(x = factor(Year), y = Value, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +  # Separated bars
  scale_fill_manual(values = c("steelblue", "darkorange")) +  # Custom colors
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  # Ensures proper spacing
  labs( x = "Year", y = "Value ('000)", fill = "Value Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Scottish Fleet Size ----
scotland_fleet <- read_excel("Fisheries.xlsx", 
                       sheet = "agg scottish fleet") 
View(scotland_fleet)

scotland_fleet <- scotland_fleet %>%
  mutate(size = case_when(
    size == "under10" ~ "Under 10 m",
    size == "over10" ~ "Over 10 m",
    size == "all" ~ "All Vessels"
  ))

ggplot(data = scotland_fleet, aes(x = year, y = fleet_n, color = size, group = size)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_colour_manual(values=cbbPalette)+
  labs(x = "Year",
       y = "Fleet Size (number of vessels)",
       color = "Vessel Size") +
  theme_classic()

## Scottish Landings value and catch ----
View(scotland_long)

scotland_long <- scotland %>%
  pivot_longer(cols = c(`value 2023`, quantity), 
               names_to = "type", 
               values_to = "n") %>%
mutate(type = case_when(
  type == "value 2023" ~ "Value (million £)",
  type == "quantity" ~ "Catch (tonnes)"
))

View(scotland_long)

ggplot(data = scotland_long, aes(x = year, y = n, color = type, group = type)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_colour_manual(values=cbbPalette)+
  labs(x = "Year",
       y = "Value ('000)",
       color = "Value Type") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, max(scotland_long$year, na.rm = TRUE), by = 1))


## Plot value/catch ----
total_value_catch <- read_excel("Fisheries.xlsx", 
                             sheet = "Value.Catch")

value_catch <- total_value_catch %>%
  filter(Species %in% c("Cuttlefish","Lobsters", "Mackerel", "Cod", "Sole", "Total All Species"))%>%
  select(-c(2:6))

value_catch_long <- value_catch %>%
  pivot_longer(cols = -Species, names_to = "Year", values_to = "n")

# Convert Year to numeric (if necessary)
value_catch_long$Year <- as.numeric(value_long$Year)
View(value_catch_long)

#colour palatte colour blind freindly
cbbPalette <- c("#009E73", "#F0E442",  "#D55E00", "#CC79A7", "#0072B2")

ggplot(data = value_catch_long, aes(x = Year, y = n, color = Species, group = Species)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_colour_manual(values=cbbPalette)+
  labs(x = "Year",
       y = "Value per Quantity Caught (£ million/Tonnes)",
       color = "Species") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, max(value_long$n, na.rm = TRUE), by = 5))+
  scale_x_continuous(breaks = seq(0, max(value_long$Year, na.rm = TRUE), by = 1))


## Value per catch in scottish fleet----

ggplot(scotland, aes(x = factor(year), y = `value/quantity`)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs( x = "Year", y = "Value per ton caught (million £/ton )") +
  theme_bw()

## Scotland ratios ----
scotland$small.big.boat.ration<- scotland$under10_n/scotland$over10_n
scotland$small.big.gt.ratio<- scotland$under10_GT/scotland$over10_GT

plot(scotland$small.big.boat.ration~scotland$year)
plot(scotland$small.big.gt.ratio~scotland$year)
head(scotland)

scotland_ratios_long <- scotland %>%
  pivot_longer(cols = c(small.big.boat.ration, small.big.gt.ratio), 
               names_to = "Category", 
               values_to = "Amount")

scotland_ratios_long <- scotland_ratios_long %>%
mutate(type = case_when(
    Category == "small.big.boat.ration" ~ "Number of boats (n)",
    Category == "small.big.gt.ratio" ~ "Boat capacity (GT)"
  ))

View(scotland_ratios_long)

ggplot(data = scotland_ratios_long, aes(x = year, y = Amount, color = type)) +
  geom_line(linewidth = 1) + 
  geom_point() +
   facet_wrap(~ type, scales = "free_y")+
  scale_color_manual(values = c("#F0E442", "#0072B2")) + 
  labs(x = "Year",
       y = "Ratio of under 10 m to over 10 m boats in the Scottish fleet") +
  theme_classic() +
  theme(legend.position = "none")

                     
boatgt.ratio<- ggplot(data = scotland, aes(x = year, y = small.big.gt.ratio)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  labs(x = "Year",
       y = "Ratio of under 10 m to over 10 m boat capacity in the Scottish fleet (GT)") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, max(value_long$n, na.rm = TRUE), by = 0.1))+
  scale_x_continuous(breaks = seq(0, max(value_long$Year, na.rm = TRUE), by = 1))

boatgt.ratio

## Scotland effort? ----
plot(scotland$`value/quantity`~ scotland$year)

ggplot(data = scotland, aes(x = year, y = `value/quantity`)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_colour_manual(values=cbbPalette)+
  labs(x = "Year",
       y = "Value per Quantity Caught (£ million/Tonnes)") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, max(value_long$Year, na.rm = TRUE), by = 1))


View(value_and_catch_total_long)
