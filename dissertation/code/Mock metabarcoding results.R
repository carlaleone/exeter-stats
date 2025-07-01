# Mock Metabarcoding results
# 16 May 2025
# Carla Leone

### Load data and packages ----
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable, readr, vegan)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
meta <- read_excel("data/mock_data.xls") #use read csv next time
meta$Duration <- as.numeric(gsub("w", "", meta$Duration))
meta

###----
###----
### Exploring the number of reads ----
# summary table for the number of Reads
summary_meta <- meta %>%
  group_by(Duration, Temperature) %>%
  summarize(
    Mean_Read = mean(Reads, na.rm = TRUE),
    SD_Read = sd(Reads, na.rm = TRUE),
    Count = n()
  )
summary_meta

#plot
readnumber.plot<- ggplot(summary_table, aes(x = Duration, y = Mean_Read, color = Temperature,fill = Temperature, group = Temperature)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of Reads"
  ) +
  theme_classic()

readnumber.plot

boxplot(meta$Reads ~ meta$Duration)
# seems like fewer reads in week 4

###----
###----
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
summary_taxa

### ----
###----
### Initial Plots for number of taxa of different levels ----

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

### ----
### ----
### Linear Model Analysis----

# I want to know whether there is a statistically significant difference in the number of species/order/family being detected

summary(glm(unique_species ~ Duration+Temperature, data = summary_taxa, family = quasipoisson))
# not significant

summary(lm(unique_orders ~ Duration*Temperature, data = summary_taxa))
# not significant

summary(lm(unique_families ~ Duration*Temperature, data = summary_taxa))
# not significant




### ----
### ----
### Community analysis ----
meta$SampleID<- paste0(substr(meta$Temperature, 1, 1), meta$Duration)
meta
meta$SampleID<- paste(meta$SampleID, meta$Replicate, sep = ".")
meta

otu_long<- subset(meta, select = c(SampleID, Species, Reads))
otu_long


# then we want to pivot the data set to make it wide, so that each species is in its own columns. 
otu_wide<- otu_long %>% 
  pivot_wider(
    names_from = Species, # columns are the names from the fish families
    values_from = Reads, # values come from the quantity column measured earlier
    values_fn = mean,  # taking the mean of the repeated values
    values_fill = 0) 
otu_wide

# make sure sample ID is the row names not an actual row
otu_wide<- otu_wide %>%
  column_to_rownames(var = "SampleID")
otu_wide

#make another data frame with the different categories for each Sample ID
treatments<- meta %>%
  distinct(SampleID, Temperature, Duration)
treatments


# bray curtis matrix of dissimilarity
otu_dist<- vegdist(otu_wide, method="bray")
summary(otu_dist)

### ----
### ----
### Trying CCA ----
# make the otu wide into a matrix for the cca model
View(otu_wide)
otu_matrix<- as.matrix(otu_wide)
View(otu_matrix)

# do a hellinger transformation because we are using raw counts: 
otu_hell <- decostand(otu_matrix, method = "hellinger")
out_hell

# create the cca model with interactions and use the treatments data frame for grouping
cca_model_interaction <- cca(otu_hell ~ Duration * Temperature, data = treatments)
summary(cca_model_interaction)
anova(cca_model_interaction)
# initial visualization
plot(cca_model, display = c("sites", "species"))

# model cca without interaction term
cca_model_basic<- cca(otu_hell ~ Duration + Temperature, data = treatments)
summary(cca_model_basic)
anova(cca_model_basic)

#adjusted r2 for the two models:
RsquareAdj(cca_model_basic)
RsquareAdj(cca_model_interaction)

# does interaction term improve the model?
anova(cca_model_basic, cca_model_interaction, by = "terms", permutations = 999)

# do each of the models explain the variation in the data?:
anova(cca_model_basic, permutations = 999)  # Test overall model without interaction
anova(cca_model_interaction, permutations = 999)  # Test overall model with interaction
### ----
### ----
### Heat Maps ----
# Select only the frozen samples
frozen <- meta %>%
  filter(Temperature == "Frozen") %>%
  select(c(Reads, Duration, Species))

#frozen<- frozen %>%
#mutate(detected = ifelse(Reads > 0, 1, 0)) %>%
#  select(-Reads) #If you want to only have number of times detected rather than read number
frozen

# make frozen wide format, with species as row names and duration as column names
frozen_wide<- frozen %>% 
  pivot_wider(
    names_from = Species, # columns are the names from the fish families
    values_from = Reads, # values come from the quantity column measured earlier
    values_fn = mean,  # taking the mean of the repeated values
    values_fill = 0) 
frozen_wide

#make species the name column
frozen_wide<- frozen_wide %>%
  column_to_rownames(var = "Duration")

# make it a matrix so its only numeric and the heatmap works
frozen_matrix<- as.matrix(frozen_wide)

#hellinger transformation like bizzozzero
hellinger_matrix <- decostand(frozen_matrix, method = "hellinger")
?decostand
install.packages("pheatmap")
library(pheatmap)

pheatmap(hellinger_matrix,
         cluster_rows = F,
         cluster_cols = F,
         scale = "none",
         main = "Hellinger-transformed Heatmap")

# basic heatmap
heatmap(frozen_matrix,Colv = NA, Rowv = NA,  scale="column", xlab="Weeks in Storage")

