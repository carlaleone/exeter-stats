# Mock Metabarcoding results
# 16 May 2025
# Carla Leone

### Load data and packages ----
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable, readr)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
meta <- read_excel("data/mock_data.xls") #use read csv next time
meta$Duration <- as.numeric(gsub("w", "", meta$Duration))
meta

### Exploring the number of reads----
# summary table for the number of Reads
summary_meta <- meta %>%
  group_by(Duration, Temperature) %>%
  summarize(
    Mean_Read = mean(Reads, na.rm = TRUE),
    SD_Read = sd(Reads, na.rm = TRUE),
    Count = n()
  )
View(summary_meta)

#plot
ggplot(summary_table, aes(x = Duration, y = Mean_Read, color = Temperature,fill = Temperature, group = Temperature)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of Reads"
  ) +
  theme_classic()


boxplot(meta$Reads ~ meta$Duration)
# seems like fewer reads in week 4

### Exploring OTUs----
unique(meta$Order)
# Total of 6 orders Identified

unique(meta$Family)
# Total of 7 families Identified

unique(meta$Species)
# Total of 14 species Identified

#summary of taxa


summary_taxa <- meta %>%
  group_by(Duration, Temperature, Replicate) %>%
  summarise(
    unique_orders = n_distinct(Order),
    unique_families = n_distinct(Family),
    unique_species = n_distinct(Species)
  )

summary_taxa$Duration <- as.numeric(gsub("w", "", summary_taxa$Duration))
View(summary_taxa)


### Initial Plots for number of taxa of differen levels ----


#plot the number of species
n_species<- ggplot(summary_taxa, aes(x = Duration, y = unique_species, color = Temperature,fill = Temperature, group = Temperature)) +
  geom_smooth(method= glm, alpha = 0.2) +
  #geom_point() +
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of species detected"
  ) + theme_classic()

#plot the nunber families
n_family<- ggplot(summary_taxa, aes(x = Duration, y = unique_families, color = Temperature,fill = Temperature, group = Temperature)) +
  geom_smooth(method= glm, alpha = 0.2) +
  #geom_point() +
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of families detected"
  ) + theme_classic()

#plot the number of orders
n_order<- ggplot(summary_taxa, aes(x = Duration, y = unique_orders, color = Temperature,fill = Temperature, group = Temperature)) +
  geom_smooth(method= glm, alpha = 0.2) +
  #geom_point() +
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of orders detected"
  ) + theme_classic() 

library(gridExtra)
grid.arrange(n_species, n_family, n_order, ncol = 3)

### Linear Model Analysis----

# I want to know whether there is a statistically significant difference in the number of species/order/family being detected

summary(glm(unique_species ~ Duration+Temperature, data = summary_taxa, family = quasipoisson))
# not significant

summary(lm(unique_orders ~ Duration*Temperature, data = summary_taxa))
# not significant

summary(lm(unique_families ~ Duration*Temperature, data = summary_taxa))
# not significant




### Community analysis ----
meta$SampleID<- paste0(substr(meta$Temperature, 1, 1), meta$Duration)
meta
meta$SampleID<- paste(meta$SampleID, meta$Replicate, sep = ".")
meta

species_long<- subset(meta, select = c(SampleID, Species, Reads))
View(species_long)


# then we want to pivot the data set to make it wide, so that each species is in its own columns. 
wide<- species_wide %>% 
  pivot_wider(
    names_from = Species, # columns are the names from the fish families
    values_from = Reads, # values come from the quantity column measured earlier
    values_fn = mean,  # taking the mean of the repeated values
    values_fill = 0) 
View(wide)


# Make a separate data set which gives the treatment for each replicate
meta
temp_groups<- meta %>%
  distinct(SampleID, Temperature)
temp_groups

time_groups <- meta %>%
  distinct(SampleID, Duration)
time_groups
