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


#----
#----
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
View(all_samples)

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
    BP_length = sum(`Align Len`, na.rm = T),
    Count = n()
  )
View(summary_meta)
sd(full_meta$`Align Len`, na.rm = T)

length(unique(full_meta$`Total read`))
#18 total detections

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
### Including NAs: NAs can be added as 0s----
full_meta <- full_meta %>%
  replace_na(list(`Total read` = 0))

View(full_meta)
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

View(sp_rich)

sp_rich$duration<- as.numeric(sp_rich$duration)
sp_rich$richness<- as.numeric(sp_rich$richness)
sp_rich$Temperature <- sp_rich$temperature


#plot species richness
meta_richness_plot<- ggplot(sp_rich, aes(x = duration, y = richness, color = Temperature,fill = Temperature, group = Temperature)) +
  geom_smooth(method= glm, method.args = list(family = poisson(link = "log")), alpha = 0.2)+ 
  geom_point(data = sp_rich, shape = 21,color = "black", stroke = 0.7, position = position_jitterdodge(), size = 2.0) +
  #geom_point(data = na_meta,
   #          aes(shape = `NA`, color = Treatment),
    #         position = position_jitterdodge(), size = 3.8) +  # X points
  labs(
    x = "Time (Weeks of storage)",
    y = "Number of species detected",
   # shape = NULL
  ) + # Use in a ggplot2 chart:
  scale_colour_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +  # line color
  scale_fill_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +    # ribbon fill
  #scale_colour_paletteer_d("lisa::BridgetRiley") +
 # scale_fill_paletteer_d("lisa::BridgetRiley") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8))+
  scale_y_continuous(breaks = c(0,1,2,3))+
  theme_classic() +
  scale_shape_manual(values = c("No species detected" = 4)) +
  theme(text = element_text(size = 15))


meta_richness_plot

#night not need this
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
  #scale_shape_manual(values = c("No species detected" = 4)) +
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
### Model sp richness ----
hist(sp_rich$richness)
richness_glm<- glm(richness~ duration + temperature, data= sp_rich, family = poisson (link = log))
summary(richness_glm)

35.455/37
# dispersion parameter

richness_simple<- glm(richness~ duration, data= sp_rich, family = poisson (link = log))
summary(richness_glm)
anova(richness_simple, richness_glm,  test="Chisq")
anova(richness_glm)
anova(richness_simple)

drop1(richness_glm, test="Chisq")
# not statistically significantly different to 

plot(richness_glm)

#----
#----
### Plot Read counts/summarise reads ----
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

# ----
#----
### Creating community matrix ----
# Select only the necessary columns
meta_long<- full_meta %>%
  select(`Sample ID`, `Species`, `Total read`)
View(meta_long)

# then we want to pivot the data set to make it wide, so that each species is in its own columns. 
meta_wide<- meta_long %>% 
  pivot_wider(
    names_from = `Species`, # columns are the names from the fish families
    values_from = `Total read`, # values come from the quantity column measured earlier
    values_fn = sum,  # taking the mean of the repeated values
    values_fill = 0) 

meta_wide <- meta_wide %>% select(-`NA`)
View(meta_wide)

# make sure sample ID is the row names not an actual row
meta_wide<- meta_wide %>%
  column_to_rownames(var = "Sample ID")
View(meta_wide_clean)

#make another data frame with the different categories for each Sample ID
treatments<- meta %>%
  distinct(`Sample ID`, temperature, duration)
treatments

# treatments including only the data that has actual results
treatments_clean <- treatments %>%
  filter(`Sample ID` %in% c("A0_2", "A0_4", "A1_1", "A4_2", "A4_3", "A4_4", "A8_1", "A8_2", "A8_4", "FR0_1", "FR2_1", "FR2_3", "FR8_4"))
View(treatments_clean)

# Making the matrix relative abundances because sequencing depth can be different for different samples
meta_ra <- decostand(meta_wide, "hellinger") #hellinger transformation is also an option, it is the square root of the total
?decostand()
View(otu_ra)

# bray curtis matrix of dissimilarity
otu_dist<- vegdist(otu_wide, method="bray")
summary(otu_dist)
# ----
# ----
### SAC trials ----
install.packages("BiodiversityR")
library(BiodiversityR)

### SAC with accumcomp ----

# make row names the first column for treatments, and make sure you have a column with read counts
treatments_sac<- treatments %>%
  column_to_rownames(var = "Sample ID")
View(treatments_sac)

View(meta_wide)


# For the duration treatment
Accum.dur <- accumcomp(meta_wide, y=treatments_sac, factor='duration', 
                       method='exact', conditioned=FALSE, plotit=FALSE)
?accumcomp

accum.long.dur <- accumcomp.long(Accum.dur, ci=NA, label.freq=5)

duration_sac_plot <- 
  ggplot(data = accum.long.dur, aes(x = Sites, y = Richness)) + 
  geom_ribbon(aes(ymin = LWR, ymax = UPR, fill = Grouping), alpha = 0.1, colour = NA) +
  geom_line(aes(colour = Grouping), size = 0.8) +
  scale_colour_manual(values = c("0" = "#E69F00", "1" = "#0072B2", "2" = "#009E73", "4" = "#D55E00", "8" = "#CC79A7")) +  # line color
  scale_fill_manual(values = c("0" = "#E69F00", "1" = "#0072B2", "2" = "#009E73", "4" = "#D55E00", "8" = "#CC79A7")) +    # ribbon fill
  labs(x = "Samples", y = "Species Richness", colour = "Duration", fill = "Duration") +
  theme_classic()

duration_sac_plot


# For the temperature treatment
Accum.temp <- accumcomp(meta_wide, y=treatments_sac, factor='temperature', 
                       method='exact', conditioned=FALSE, plotit=FALSE)

accum.long.temp <- accumcomp.long(Accum.temp, ci=NA, label.freq=5)

temp_sac_plot <- 
  ggplot(data = accum.long.temp, aes(x = Sites, y = Richness)) + 
  geom_ribbon(aes(ymin = LWR, ymax = UPR, fill = Grouping), alpha = 0.1, colour = NA) +
  geom_line(aes(colour = Grouping), size = 1.2) +
  scale_colour_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +  # line color
  scale_fill_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +    # ribbon fill
  labs(x = "Samples", y = "Species Richness", colour = "Temperature", fill = "Temperature") +
  theme_classic()

temp_sac_plot



#----
#----
### SAC with accumcomp only FROZEN SAMPLES----
#subset the data so that you have one for each temperature now creating fro meta frozen
meta_frozen<- meta_wide
meta_frozen$Sample.ID <- rownames(meta_frozen)
meta_frozen <- meta_frozen[grepl('FR', meta_frozen$Sample.ID),]
View(meta_frozen)
meta_frozen <- subset(meta_frozen, select = -Sample.ID )

#subset treatments frozen
treatments_frozen<- treatments_sac
treatments_frozen<- treatments_frozen[grepl('FR', treatments_frozen$Sample.ID),]
View(treatments_frozen)

#run accumcomp on meta wide using temp and the read counts as the scale
Accum.FR <- accumcomp(meta_frozen, y=treatments_frozen, factor='duration',
                     method='exact', conditioned=FALSE, plotit=FALSE)


#long format
accum.long.FR <- accumcomp.long(Accum.FR, ci=NA, label.freq=1)
head(accum.long2)

fr_sac_reads_plot<- ggplot(data=accum.long.FR, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  geom_line(aes(colour=Grouping), size=1.2) +
  geom_ribbon(aes(colour=Grouping, fill=after_scale(alpha(colour, alpha=0.3))), 
              show.legend=FALSE) + 
  labs(x = "Read Count", y = "Species", colour = "Duration", shape = "Duration") +
  theme_classic()
fr_sac_reads_plot

#----
#----
### PERMANOVA ----
# need to remove rows that have all 0s
meta_wide_clean <- meta_wide[rowSums(meta_wide) > 0, ]
View(meta_wide_clean)

# dont need to do tranformation before PERMANOVA because bray-curtis already calculate rel abundance...
# but need to check for dispersion in groups:
betadispersion.temp <- betadisper(vegdist(meta_wide_clean, method = "bray"), treatments_clean$temperature)
permutest(betadispersion.temp)
# there is a difference in dispersion between groups? p = 0.044 so almost unequal dispersion

betadispersion.duration <- betadisper(vegdist(meta_wide_clean, method = "bray"), treatments_clean$duration)
permutest(betadispersion.duration)


anova(betadispersion)
plot(betadispersion)

# do the hellinger transformation
otu_hel <- decostand(otu_matrix, method = "hellinger") 
# calculate the distance matrix
otu_dist<- vegdist(otu_wide, method="bray")


# do the PERMANOVA
adonis2(meta_wide_clean ~ duration*temperature , data = treatments_clean, permutations = 9999)

#----
#----
### CCA with interaction term ----
# make the otu wide into a matrix for the cca model
meta_matrix<- as.matrix(meta_wide_clean)
View(meta_matrix)

# use hellinger transformation from earlier
meta_hel <- decostand(meta_matrix, method = "hellinger") 

#make duration a factor (says to do so if you suspect that the relationship is not linear)
treatments$Duration <- as.factor(treatments$Duration)

#create the cca model with the interaction
cca_model_i <- cca(meta_hel ~ temperature* duration, data = treatments_clean)
plot(cca_model)

#perform a permutational anova
anova(cca_model_i, by = "term",  permutations = 9999)  # tests each term: Temp, Time, and interaction

anova(cca_model_i)
# another visualization
plot(cca_model, display = c("sites", "species"))

#----
#----
### CCA with interaction Model Diagnostics ----
# assess VIF for collinearity, if greater than 10 suggests high collinearity
vif.cca(cca_model_i)

# adjusted r2 of the model
RsquareAdj(cca_model_i)
# 0.06211379

goodness(cca_model_i, display = c("species", "sites"),
         model = c("CCA", "CA"), summarize = FALSE, addprevious = FALSE)

spenvcor(cca_model)
#----
#----
### CCA without interaction term ----
cca_model_b<- cca(meta_hel ~ temperature+ duration, data = treatments_clean)

#perform a permutational anova
anova(cca_model_b, by = "term",  permutations = 9999)  # tests each term: Temp, Time, and interaction

anova(cca_model_b)
# another visualization
plot(cca_model_b, display = c("sites", "species"))
#----
#----
### CCA without interaction Model Diagnostics ----
# assess VIF for collinearity, if greater than 10 suggests high collinearity
vif.cca(cca_model_b)

# adjusted r2 of the model
RsquareAdj(cca_model_b)
# 0.01647461

goodness(cca_model_b, display = c("species", "sites"),
         model = c("CCA", "CA"), summarize = FALSE, addprevious = FALSE)

spenvcor(cca_model)

# does interaction term improve the model?
anova(cca_model_b, cca_model_i, by = "terms", permutations = 999)

# do each of the models explain the variation in the data?:
anova(cca_model_b, permutations = 999)  # Test overall model without interaction
anova(cca_model_i, permutations = 999)  # Test overall model with interaction
# ----
# ----
### Plot the CCA ----

#----
#----

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
