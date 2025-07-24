# Metabarcoding results
# 15 July 2025
# Carla Leone

### Load data and packages ----
install.packages("pacman")
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable, readr, vegan, BiodiversityR, RColorBrewer)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
metabarcoding_data <- read_csv("data/metabarcoding_results.csv") 
View(metabarcoding_data)

#----
#----
### Cleaning the data ----
meta <- metabarcoding_data %>% drop_na(Species)
#remove na species


# remove species that are not feasible or usable for the analysis
meta <- meta %>%
  filter(!Species %in% c("Homo sapiens", "Canis lupus", "Skip", "Bos primigenius", "Danio rerio"))


# add samples that would have had no reads
all_samples_B<- read_xlsx("data/all samples.xlsx") # with blanks
all_samples<- all_samples_B %>%
  filter(!`Sample ID` %in% c("B0", "B1", "B2", "B4", "B8"))
View(all_samples)

#Join all samples and metabarcoding data into one data frame
full_meta <- all_samples %>%
  left_join(meta, by = "Sample ID")
View(full_meta)


data <- full_meta %>%
  mutate(
    temperature = str_extract(`Sample ID`, "^[A-Z]+"),  # A, B, FR
    duration = as.numeric(str_remove_all(
      str_remove(`Sample ID`, "^[A-Z]+"),  
      "_\\d+$"                             
    )),
    replicate = str_extract(`Sample ID`, "(?<=_)\\d+")
  )

#create new columns for treatments
View(data)

meta <- data %>%
  mutate(temperature = recode(temperature,
                            "A" = "Ambient",
                            "B" = "Blank",
                            "FR" = "Frozen",
                            "P" = "P"))

View(meta)

# remove the blank because it had a detection, but that detection was for week 1 and no other week 1 sample had that detection.
meta <- meta %>%
  filter(!temperature %in% c("Blank"))

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

# how many species did we detect in total?
unique(meta$Species)
# 10 unique species

# how many total detections?
length(unique(full_meta$`Total read`))
#18 total detections

#13 samples had detections

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

# plot read counts
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

#----
#----
### Summary figures for read numbers (appendix) ----
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



#----
#----
### Species Richness ----
head(meta)

#create the species richness data frame by counting number of unique species detected per sample
sp_rich <- meta %>%
  group_by(`Sample ID`, temperature, duration) %>%
  summarise(richness = n_distinct(`Species`,na.rm = TRUE)) %>%
  ungroup()


# ensure richness and duration are numeric
sp_rich$duration<- as.numeric(sp_rich$duration)
sp_rich$richness<- as.numeric(sp_rich$richness)
# make temperature upper case: for plotting
sp_rich$Temperature <- sp_rich$temperature

#----
#----
### Model species richness ----
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
### Plot species richness ----

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
#----
#----
### Create community matrix ----
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

# need to remove rows that have all 0s
meta_wide_clean <- meta_wide[rowSums(meta_wide) > 0, ]
View(meta_wide_clean)

#make another data frame with the different categories for each Sample ID
treatments<- sp_rich %>%
  distinct(`Sample ID`, temperature, duration)
treatments

# treatments including only the data that has actual results
treatments_clean <- treatments %>%
  filter(`Sample ID` %in% c("A0_2", "A0_4", "A1_1", "A4_2", "A4_3", "A4_4", "A8_1", "A8_2", "A8_4", "FR0_1", "FR2_1", "FR2_3", "FR8_4"))
View(treatments_clean)

# ----
# ----
### PERMANOVA ----
# dont need to do tranformation before PERMANOVA because bray-curtis already calculate rel abundance...
# but need to check for dispersion in groups:
betadispersion.temp <- betadisper(vegdist(meta_wide_clean, method = "bray"), treatments_clean$temperature)
permutest(betadispersion.temp)
# there is a difference in dispersion between groups? p = 0.05 so almost unequal dispersion

betadispersion.duration <- betadisper(vegdist(meta_wide_clean, method = "bray"), treatments_clean$duration)
permutest(betadispersion.duration)


anova(betadispersion)
plot(betadispersion)


# do the PERMANOVA
adonis2(meta_wide_clean ~ duration*temperature , data = treatments_clean, permutations = 9999)

#----
#----
### CCA with interaction term ----
#create new treatments data frame with only data needed
View(treatments_clean)
treatments_cca<- treatments_clean %>%
  column_to_rownames(var = "Sample ID")

# make the otu wide into a matrix for the cca model
meta_matrix<- as.matrix(meta_wide_clean)
View(meta_matrix)

# use hellinger transformation (Bizzozero)
meta_hel <- decostand(meta_matrix, method = "hellinger") 
View(meta_hel)

#create the cca model with the interaction
cca_model_i <- cca(meta_hel ~ duration*temperature, data = treatments_cca)

?cca
?cca.object
#perform a permutational anova
anova(cca_model_i, by = "term",  permutations = 9999)  # tests each term: Temp, Time, and interaction

anova(cca_model_i)

#----
#----
### CCA with interaction Model Diagnostics ----
# assess VIF for collinearity, if greater than 10 suggests high collinearity
vif.cca(cca_model_i)

# adjusted r2 of the model
RsquareAdj(cca_model_i)
#  0.060

goodness(cca_model_i, display = c("species", "sites"),
         model = c("CCA", "CA"), summarize = FALSE, addprevious = FALSE)

spenvcor(cca_model_i)
#----
#----
### CCA without interaction term ----
cca_model_b<- cca(meta_hel ~ duration + temperature, data = treatments_cca)

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
# -0.01747461 so model is worse than a null fit

goodness(cca_model_b, display = c("species", "sites"),
         model = c("CCA", "CA"), summarize = FALSE, addprevious = FALSE)

spenvcor(cca_model)

# does interaction term improve the model?
anova(cca_model_i, cca_model_b, by = "terms", permutations = 9999)

# do each of the models explain the variation in the data?:
anova(cca_model_b, permutations = 9999)  # Test overall model without interaction
anova(cca_model_i, permutations = 9999)  # Test overall model with interaction
# ----
# ----
### Plot the CCA ----
cca_model_i
?cca
scores.df<- as.data.frame(scores(cca_model_i, choices = c(1,2), display = "sites"))
scores<- as.data.frame(scores(cca_model_i)) 
scores.df
scores.df$site <- rownames(scores.df)
temps<- treatments_clean$temperature
scores.df$temperature<- temps


#next for the species:
species.scores <- as.data.frame(scores(cca_model_i, choices = c(1,2), display = "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)


# to plot the hulls
grp.f <- scores.df[scores.df$temperature == "Frozen", ][chull(scores.df[scores.df$temperature == 
                                                                   "Frozen", c("CCA1", "CCA2")]), ]  # hull values for grp frozen
grp.a <- scores.df[scores.df$temperature == "Ambient", ][chull(scores.df[scores.df$temperature == 
                                                                   "Ambient", c("CCA1", "CCA2")]), ]  # hull values for grp ambient

hull.data <- rbind(grp.f, grp.a)  #combine grp.a and grp.b
hull.data

ggplot() + 
  geom_polygon(data=hull.data,aes(x=CCA1,y=CCA2,fill=temperature,group=temperature),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=CCA1,y=CCA2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=scores.df,aes(x=CCA1,y=CCA2,shape=temperature,colour=temperature),size=4) + # add the point markers
  scale_colour_manual(values=c("Frozen" = "blue", "Ambient" = "red")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())



ggplot() +
  geom_polygon(data=hull.data,aes(x=CCA1,y=CCA2,fill=temperature,group=temperature),alpha=0.30) +# add the convex hulls
  labs(fill = "temperature") +
  # geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size= 2, alpha=0.5) +  # add the species labels
  geom_point(data=scores.df,aes(x=CCA1,y=CCA2,colour=temperature),size=3) + # add the point markers
  geom_text(data=species.scores,aes(x=CCA1,y=CCA2,label = species),size=2,vjust=0) +  # add the site labels
  #scale_colour_manual(values=Colours) +
  #scale_fill_manual(values=Colours) +
  # scale_x_continuous(limits = c(-1.4, 3), breaks = c(-1,0,1,2,3)) +
  # scale_y_continuous(limits = c(-1.4, 1.2), breaks = c(-1,-0.5,0, 0.5 ,1)) +
  scale_x_continuous(limits = c(-4, 1.5), breaks = c(-3,-2,-1, 0, 1)) +
  scale_y_continuous(limits = c(-2, 2), breaks = c(-2,-1,0, 1,2)) +
  #coord_equal() +
  theme_classic() +
  #ggtitle("Broadband Presence/Absence") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(size = 1.5),  # increase axis line thickness
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),  # increase legend text size
        legend.title = element_text(size = 18),
        legend.background = element_rect(color = "grey", size = 0.5))+
  # legend.position = "top")+
  guides(colour = FALSE)
) 

ordihull(cca_model_i, # the nmds we created
         groups= treatments_cca$temperature, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:2, # shading the plygons
         label = FALSE #removing labels from the plygons
) 
#----
#----
### Heat maps with total hellinger transformation? ---- 


# Assume df_species contains only the species read counts
hellinger_all <- decostand(meta_wide_clean, method = "hellinger")
View(hellinger_all)

# Step 2: Add metadata back (e.g., temperature, duration)
hellinger_all$temperature <- treatments_clean$temperature
hellinger_all$duration <- treatments_clean$duration

# Step 3: Split by temperature group
hell_ambient <- hellinger_all[hellinger_all$temperature == "Ambient", ]
hell_frozen  <- hellinger_all[hellinger_all$temperature == "Frozen",  ]

View(hell_ambient)
#----
#----
### Heat Map for frozen samples ----
#using the hellinger transformation within the temperature group
# Select only the frozen samples
meta_frozen<- meta_wide # copy meta wide
meta_frozen$`Sample ID` <- rownames(meta_frozen) # extract the sample id column
rownames(meta_frozen) <- NULL # remove current row names
meta_frozen <- meta_frozen %>% #merge the treatments
  left_join(treatments, by = "Sample ID")
meta_frozen <- meta_frozen[grepl('FR', meta_frozen$Sample.ID),] #select only frozen treatments
meta_frozen$duration<- as.factor(meta_frozen$duration)
View(meta_frozen)
species_cols <- colnames(meta_frozen)[1:10]

meta_frozen <- meta_frozen %>%
  group_by(duration) %>%
  summarise(across(all_of(species_cols), sum, na.rm = TRUE))

View(meta_frozen)

#make species the name column

frozen_wide<- meta_frozen %>%
  column_to_rownames(var = "duration")

#frozen_wide <- frozen_wide[, colSums(frozen_wide != 0, na.rm = TRUE) > 0]

View(frozen_wide)

# make it a matrix so its only numeric and the heatmap works
frozen_matrix<- as.matrix(frozen_wide)
View(frozen_matrix)
#hellinger transformation like bizzozzero
fr_hel_matrix <- decostand(frozen_wide, method = "hellinger")
View(fr_hel_matrix)
?decostand
install.packages("pheatmap")
library(pheatmap)

fr_hel_matrix_t <- t(fr_hel_matrix) #transpose the matrix
fr_hel_matrix_na <- fr_hel_matrix_t # create new matrix for the NAs
fr_hel_matrix_na[fr_hel_matrix_na == 0] <- NA # record 0s as NAs
my_colors_fr <- colorRampPalette(brewer.pal(9, "Blues"))(100) #select the colour ramp
breaks <- seq(0, 1, length.out = 100)
p_fr<- pheatmap(fr_hel_matrix_na,
               cluster_rows = F,
               cluster_cols =F,
               breaks = breaks,
               labels_row = rownames(fr_hel_matrix_na),
               scale = "none",
               color = my_colors,
               na_col = "transparent",
               show_rownames = F,
               angle_col = 0,
               legend = F,
               main = "Frozen",
               cellwidth = 30,    # width of each column
               cellheight = 27)


#----
#----
### Heat Map for ambient samples ----
# Select only the frozen samples
meta_ambient<- meta_wide # copy meta wide
meta_ambient$`Sample ID` <- rownames(meta_ambient) # extract the sample id column
rownames(meta_ambient) <- NULL # remove current row names
meta_ambient <- meta_ambient %>% #merge the treatments
  left_join(treatments, by = "Sample ID")
meta_ambient <- meta_ambient[grepl('A', meta_ambient$Sample.ID),] #select only frozen treatments
meta_ambient$duration<- as.factor(meta_ambient$duration)
View(meta_ambient)
species_cols <- colnames(meta_ambient)[1:10]

meta_ambient <- meta_ambient %>%
  group_by(duration) %>%
  summarise(across(all_of(species_cols), sum, na.rm = TRUE))

#make species the name column

ambient_wide<- meta_ambient %>%
  column_to_rownames(var = "duration")

#ambient_wide <- ambient_wide[, colSums(ambient_wide != 0, na.rm = TRUE) > 0]

View(ambient_wide)

# make it a matrix so its only numeric and the heatmap works
#ambient_matrix<- as.matrix(ambient_wide)

#hellinger transformation like bizzozzero
am_hel_matrix <- decostand(ambient_wide, method = "hellinger")

?decostand
install.packages("pheatmap")
library(pheatmap)

install.packages("RColorBrewer")
library(RColorBrewer)

am_hel_matrix_t <- t(am_hel_matrix) #transpose the matrix
am_hel_matrix_na <- am_hel_matrix_t # create new matrix for the NAs
am_hel_matrix_na[am_hel_matrix_na == 0] <- NA # record 0s as NAs
my_colors <- colorRampPalette(brewer.pal(9, "Oranges"))(100) #select the colour ramp
p_a<- pheatmap(am_hel_matrix_na,
         cluster_rows = F,
         cluster_cols = F,
         breaks = breaks,
         scale = "none",
         color = my_colors,
         na_col = "transparent", 
         angle_col = 0,
         main = "Ambient",
         cellwidth = 30,    # width of each column
         cellheight = 27)


# save both maps together
install.packages("gridExtra") 

library(gridExtra)

gridExtra::grid.arrange(p_fr$gtable, p_a$gtable, ncol = 2)


#----
#----
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
### Testing ranacapa package ----
library(devtools)
install.packages("devtools")
install.packages('phyloseq')
library(phyloseq)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("multtest")
BiocManager::install("phyloseq")

if (!requireNamespace("devtools", quietly = TRUE))
  install.packages('devtools')


devtools::install_github("gauravsk/ranacapa")
library(ranacapa)
ranacapa::runRanacapaApp()

?convert_anacapa_to_phyloseq

anacapra<- meta_wide_clean
anacapra<- t(anacapra)
anacapra<- as.data.frame(anacapra)
View(anacapra)
anacapra<- rownames_to_column(anacapra, "sum.taxonomy") 
View(treatments_clean)
ana.treat<- treatments_clean %>%
  column_to_rownames(var = "Sample ID")
ana.treat<- t(ana.treat)
View(ana.treat)
convert_anacapa_to_phyloseq(anacapra, ana.treat)
### Venn diagrams? ----
# ----
# ----
### Citations----
install.packages("report")
library(report)

cite_packages()
