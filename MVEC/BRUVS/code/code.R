# MVEC BRUVS Code
# Carla Leone
# 08/01/2025

## Import data and packages ----
library(readxl)
library(tidyverse)

d<- read_excel("MVEC/BRUVS/data/BRUV Data.xls")
View(d)
d <- d[, -12]

## subset data because we have some hyperabundant fish and other unnamed fish ----
d<- subset(d,d$max_n <= 25)
#remove shoals greater than 25
d<- na.omit(d)
# remove species without a name


## For relative abundance we should filter the data to contain one row per species per video. So eahc observation is only the max N ----
# group data by unique variabels for a specific video
d_maxN<- d %>%
  group_by(code, habitat, species_common) %>%
  summarise(maxN = max(max_n))

View(d_maxN)

## Calculate richness ----
d_sp_richness<- d %>%
  group_by(code, habitat) %>%
  summarise(sp_richness = length(unique(species_common)))

d_sp_richness<- d_sp_richness%>%
  group_by(habitat) %>%
  summarise(n=n(), mean = mean(sp_richness), sd = sd(sp_richness), se=sd/sqrt(n))

View(d_sp_richness)
