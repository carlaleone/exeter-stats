# Mock Metabarcoding results
# 16 May 2025
# Carla Leone

### Load data and packages ----
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable)
getwd()
mock_data <- read_excel("data/mock_data.xls")
View(mock_data)

mock_data$Duration <- as.numeric(gsub("w", "", mock_data$Duration))


### Exploring the data----
#summary table for the number of Reads
summary_table <- mock_data %>%
  group_by(Duration, Temperature) %>%
  summarize(
    Mean_Read = mean(Reads, na.rm = TRUE),
    SD_Read = sd(Reads, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  )
View(summary_table)

#plot
ggplot(summary_table, aes(x = Duration, y = Mean_Read, color = Temperature,fill = Temperature, group = Temperature)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of Reads"
  ) +
  theme_classic()


boxplot(mock_data$Reads ~ mock_data$Duration)
# seems like fewer reads in week 4

unique(mock_data$Order)
# Total of 6 orders Identified

unique(mock_data$Family)
# Total of 7 families Identified

unique(mock_data$Species)
# Total of 14 species Identified

#summary of taxa


summary_taxa <- mock_data %>%
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
species_wide1<- subset(mock_data, select = c(Duration, Temperature, Species, Reads, Replicate))
View(species_wide)

species_wide<- species_wide1 %>% 
  group_by(Duration, Temperature, Species) %>% #grouping by fish family
  summarize(Read = mean(Reads), #will return the average number of fish for each family at each reef
            Duration  = Duration,
            Temperature = Temperature,
            Species = Species) %>% 
  ungroup()

View(species_wide)

# then we want to pivot the data set to make it wide, so that each species is in its own columns. 
wide<- species_wide %>% 
  pivot_wider(
    names_from = Species, # columns are the names from the fish families
    values_from = Read, # values come from the quantity column measured earlier
    values_fn = mean,  # taking the mean of the repeated values
    values_fill = 0) 
View(wide)
