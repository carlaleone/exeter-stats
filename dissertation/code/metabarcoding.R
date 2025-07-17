# Metabarcoding results
# 15 July 2025
# Carla Leone

### Load data and packages ----
install.packages("pacman")
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable, readr, vegan)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
metabarcoding_data <- read_csv("data/metabarcoding_results.csv") 
View(metabarcoding_data)


###----
###----
### Cleaning the data ----
meta <- metabarcoding_data %>% drop_na(Species)
#remove na species

meta <- meta %>%
  filter(!Species %in% c("Homo sapiens", "Canis lupus", "Skip", "Bos primigenius", "Danio rerio"))
# remove species that are not feasible or useable for the analysis

# add samples that would have had no reads
all_samples_B<- read_xlsx("data/all samples.xlsx") # with blanks
all_samples<- all_samples_B %>%
  filter(!`Sample ID` %in% c("B0", "B1", "B2", "B4", "B8"))

full_meta <- all_samples %>%
  left_join(meta, by = "Sample ID")
View(full_meta)


df <- full_meta %>%
  mutate(
    temperature = str_extract(`Sample ID`, "^[A-Z]+"),  # A, B, FR
    duration = as.numeric(str_remove_all(
      str_remove(`Sample ID`, "^[A-Z]+"),  # Remove prefix
      "_\\d+$"                             # Remove _replicate
    )),
    replicate = str_extract(`Sample ID`, "(?<=_)\\d+")
  )

#create new columns for treatments

View(df)

meta <- df %>%
  mutate(temperature = recode(temperature,
                            "A" = "Ambient",
                            "B" = "Blank",
                            "FR" = "Frozen",
                            "P" = "P"))

View(meta)

meta <- meta %>%
  filter(!temperature %in% c("Blank"))
# remove the blank because it had a detection, but that detection was for week 1 and no other week 1 sample had that detection.

unique(meta$Species)
# 10 unique species

#----
#----
### Exploring the number of reads ----
# summary table for the number of Reads
summary_meta <- meta %>%
  group_by(duration, temperature) %>%
  summarize(
    Mean_Read = mean(`Total read`, na.rm = TRUE),
    SD_Read = sd(`Total read`, na.rm = TRUE),
    Total_Read = sum(`Total read`, na.rm = TRUE),
    Count = n()
  )
View(summary_meta)


# summary of unfiltered results. 

summary_all_positive<- metabarcoding_data %>% drop_na(Species)

summary_all_positive<- summary_all_positive %>%
  group_by(`Sample ID`) %>%
  summarize(
    Total_Read = sum(`Total read`, na.rm = TRUE)) %>%
  ungroup()
  
summary_all_positive <-summary_all_positive %>%
  filter(!`Sample ID` %in% c("NTC_seq", "NTC_seq2", "PTC_seq", "PTC_seq2"))

View(summary_all_positive)
sum(summary_all_positive$Total_Read )
#sum of all reads = 3,304,926

# summary of filtered data
filtered_detections_summary<- meta %>%
  group_by(`Sample ID`) %>%
  summarize(
    Mean_Read = mean(`Total read`, na.rm = TRUE),
    SD_Read = sd(`Total read`, na.rm = TRUE),
    Total_Read = sum(`Total read`, na.rm = TRUE),
    Count = n()) %>%
  ungroup()

View(filtered_detections_summary)

temp_detections_summary <- meta %>%
  group_by(temperature) %>%
  summarize(
    Mean_Read = mean(`Total read`, na.rm = TRUE),
    SD_Read = sd(`Total read`, na.rm = TRUE),
    Total_Read = sum(`Total read`, na.rm = TRUE),
    Count = n()) %>%
  ungroup()

View(temp_detections_summary)

# total reads with duration
duration_detections_summary <- meta %>%
  group_by(duration) %>%
  summarize(
    Mean_Read = mean(`Total read`, na.rm = TRUE),
    SD_Read = sd(`Total read`, na.rm = TRUE),
    Total_Read = sum(`Total read`, na.rm = TRUE),
    Count = n()) %>%
  ungroup()


View(duration_detections_summary)

# total reads including duration and temp
treatment_detections_summary <- meta %>%
  group_by(duration, temperature) %>%
  summarize(
    Mean_Read = mean(`Total read`, na.rm = TRUE),
    SD_Read = sd(`Total read`, na.rm = TRUE),
    Total_Read = sum(`Total read`, na.rm = TRUE),
    Count = n()) %>%
  ungroup()


View(treatment_detections_summary)

sum(meta$`Total read`, na.rm = TRUE)
# total number of reads = 684365

boxplot(meta$`Total read` ~ meta$temperature)
boxplot(meta$`Total read` ~ meta$duration)
#----
#----
### Figures for read numbers (appendix) ----
ggplot(data = meta, aes(x = factor(temperature), y = `Total read`, fill = temperature)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  facet_wrap(~ duration, scales = "free", ncol = 2) +
  labs(
    x = "Temperature",
    y = "Read Count",
    fill = "Temperature"
  ) +
  scale_fill_manual(values = c("Ambient" = "#F4ADA7", "Frozen" = "#76B451")) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14),
    legend.position ="none"
  )

# next need to find out whether I can add 0s for the replicates that had 0 detections.
#----
#----
### Including NAs----

na_meta<- meta %>% 
  filter(is.na(`Total read`)) %>%
  mutate(`Total read` = 0.8) 
#yield give a pseudo number just for visualization = 0.1) #yield give a pseudo number just for visualization

na_meta <- na_meta %>%
  group_by(`Sample ID`, temperature, duration) %>%
  summarise(richness = mean(`Total read`)) %>%
  ungroup()

View(na_meta)

na_meta$`NA` <- "No species detected"
na_meta <- na_meta %>%
  mutate(Treatment = toupper(temperature))

#----
#----
### Species Richness ----
View(meta)
sp_rich <- meta %>%
  group_by(`Sample ID`, temperature, duration) %>%
  summarise(richness = n_distinct(`Species`,na.rm = TRUE)) %>%
  ungroup()

sp_rich <-sp_rich %>%
  filter(!richness %in% c("0"))

View(sp_rich)

sp_rich$duration<- as.numeric(sp_rich$duration)
sp_rich$richness<- as.numeric(sp_rich$richness)
sp_rich <- sp_rich %>%
  mutate(Treatment = toupper(temperature))


#plot species richness
meta_richness_plot<- ggplot(sp_rich, aes(x = duration, y = richness, color = Treatment,fill = Treatment, group = Treatment)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  geom_point(data = sp_rich, shape = 21, position = position_jitterdodge(), size = 2.3) +
  geom_point(data = na_meta,
             aes(shape = `NA`, color = Treatment),
             position = position_jitterdodge(), size = 3.8) +  # X points
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of species detected",
    shape = NULL
  ) + # Use in a ggplot2 chart:
  scale_colour_paletteer_d("lisa::BridgetRiley") +
  scale_fill_paletteer_d("lisa::BridgetRiley") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8))+
  scale_y_continuous(breaks = c(1,2,3))+
  theme_classic() +
  scale_shape_manual(values = c("No species detected" = 4)) +
  theme(text = element_text(size = 15))



meta_richness_plot <- ggplot(sp_rich, aes(x = duration, y = richness, color = Treatment, fill = Treatment, group = Treatment)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  coord_cartesian(ylim = c(0.7, NA)) +
  geom_point(data = sp_rich, shape = 21, position = position_jitterdodge(), size = 2.3) +
  geom_point(data = na_meta,
             aes(shape = `NA`, color = Treatment),
             position = position_jitterdodge(), size = 3.8) +
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of species detected",
    shape = NULL
  ) +
  scale_colour_paletteer_d("lisa::BridgetRiley") +
  scale_fill_paletteer_d("lisa::BridgetRiley") +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  scale_y_continuous(breaks = c(1, 2, 3)) +
  scale_shape_manual(values = c("No species detected" = 4)) +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  guides(
    color = guide_legend(order = 1),
    fill = "none",
    shape = guide_legend(order = 3)  # Make shape legend come last
  ) 
meta_richness_plot

?geom_point
#----
#----
### Plot Reads ----
View(filtered_detections_summary)

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
View(otu_wide)

# make sure sample ID is the row names not an actual row
otu_wide<- otu_wide %>%
  column_to_rownames(var = "SampleID")
otu_wide

#make another data frame with the different categories for each Sample ID
treatments<- meta %>%
  distinct(SampleID, Temperature, Duration)
treatments

# Making the matrix relative abundances because sequencing depth can be different for different samples
otu_ra <- decostand(otu_wide, "hellinger") #hellinger transformation is also an option, it is the square root of the total
?decostand()
View(otu_ra)

# bray curtis matrix of dissimilarity
otu_dist<- vegdist(otu_wide, method="bray")
summary(otu_dist)
### ----
### ----
### SAC trials ----
install.packages("BiodiversityR")
library(BiodiversityR)

# subset the matrix into each of the treatments
#frozen
samples_fr <- treatments$SampleID[treatments$Temperature == "Frozen"]
frozen_matrix <- otu_wide[samples_fr, ]
#ambient
samples_a <- treatments$SampleID[treatments$Temperature == "Ambient"]
a_matrix <- otu_wide[samples_a, ]
#w0
samples_0 <- treatments$SampleID[treatments$Duration == "0"]
matrix_0 <- otu_wide[samples_0, ]
#w1
samples_1 <- treatments$SampleID[treatments$Duration == "1"]
matrix_1 <- otu_wide[samples_1, ]
#w2
samples_2 <- treatments$SampleID[treatments$Duration == "2"]
matrix_2 <- otu_wide[samples_2, ]
#w4
samples_4 <- treatments$SampleID[treatments$Duration == "4"]
matrix_4 <- otu_wide[samples_4, ]

# then calculate the sac for each matrix subset
sac_freezer <- specaccum(frozen_matrix, method = "random")
sac_ambient<- specaccum(a_matrix, method = "random")

#Accum.1 <- accumcomp(otu_wide, y=treatments, factor='SampleID', 
method='exact', conditioned=FALSE, plotit=FALSE)


###----
###----
### PERMANOVA ----
# dont need to do tranformation before PERMANOVA because bray-curtis already calculate rel abundance...
# but need to check for dispersion in groups:
betadispersion <- betadisper(vegdist(otu_wide, method = "bray"), treatments$Temperature)
permutest(betadispersion)
anova(betadispersion)
plot(betadispersion)

# do the hellinger transformation
otu_hel <- decostand(otu_matrix, method = "hellinger") 
# calculate the distance matrix
otu_dist<- vegdist(otu_wide, method="bray")
# do the PERMANOVA
adonis2(otu_wide ~ Temperature * Duration, data = treatments, permutations = 9999)

###----
###----
### Trying CCA ----
# make the otu wide into a matrix for the cca model
View(otu_wide)
otu_matrix<- as.matrix(otu_wide)
View(otu_matrix)

# use hellinger transformation from earlier
otu_hel <- decostand(otu_matrix, method = "hellinger") 

#make duration a factor (says to do so if you suspect that the relationship is not linear)
treatments$Duration <- as.factor(treatments$Duration)

cca_model <- cca(otu_hel ~ Temperature * Duration, data = treatments)
plot(cca_model)
anova(cca_model, by = "term")  # tests each term: Temp, Time, and interaction

# another visualization
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


#----
#----
### Citations----
install.packages("report")
library(report)

cite_packages()
