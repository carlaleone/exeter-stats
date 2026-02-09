# Metabarcoding results: MSc Thesis Publication using new sequencing results
# 5 February 2026
# Carla Leone

### Load data and packages ----
install.packages("pacman")
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, readr, vegan, report, BiodiversityR, RColorBrewer, car, pheatmap, broom, patchwork)
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
metabarcoding_data <- read_csv("data/metabarcoding_results_new.csv") 
#View(metabarcoding_data)
#----

#----

### Clean the data ----
meta1 <- metabarcoding_data %>% drop_na(Species)
#remove na species


# remove species that are not feasible or usable for the analysis
meta1 <- meta1 %>%
  filter(!Species %in% c("Homo sapiens", "Canis lupus", "Skip", "Bos primigenius", "Danio rerio"))


# add samples that would have had no reads
all_samples_B<- read_xlsx("data/all samples.xlsx") # with blanks
all_samples<- all_samples_B %>%
  filter(!`Sample ID` %in% c("B0", "B1", "B2", "B4", "B8")) #remove blanks

#View(all_samples)

#Join all samples and metabarcoding data into one data frame
full_meta <- all_samples %>%
  left_join(meta1, by = "Sample ID")


# add the treatment groups and sample IDs 
meta2 <- full_meta %>%
  mutate(
    temperature = str_extract(`Sample ID`, "^[A-Z]+"),  # A, B, FR
    duration = as.numeric(str_remove_all(
      str_remove(`Sample ID`, "^[A-Z]+"),  
      "_\\d+$"                             
    )),
    replicate = str_extract(`Sample ID`, "(?<=_)\\d+")
  )


# write the full treatment
meta <- meta2 %>%
  mutate(temperature = dplyr::recode(temperature,
                                     "A" = "Ambient",
                                     "FR" = "Frozen"))

# final metabarcoding data sheet


#----

#----

### Summary figures for total read numbers (Appendix A) ----
boxplot_reads<- ggplot(data = meta, aes(x = factor(temperature), y = `Total read`, fill = temperature)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  #facet_wrap(~ duration, scales = "free", ncol = 2) +
  labs(
    x = "Temperature",
    y = "Read Count",
    fill = "Temperature"
  ) +
  scale_fill_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14),
    legend.position ="none"
  )

scatter_plot_reads<- ggplot(meta, aes(x = factor(duration), y = `Total read`, color = temperature)) +
  geom_jitter(width = 0.3, size = 3, alpha = 0.7) +  # adds some spread so points don't overlap
  scale_color_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +
  # scale_y_log10(labels = scales::comma) +  # log scale for better visibility
  labs(
    x = "Duration (weeks)",
    y = "Read Count",
    color = "Temperature"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


boxplot_reads + scatter_plot_reads
#----

#----

### SPECIES RICHNESS ANALYSIS----
### Species Richness Calculation ----

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


# make another species richness data set with only samples that had detections (removing 0 richness)
# used for calculations
sp_rich_clean <- sp_rich %>%
  filter(richness != 0)

#----

#----

### Model species richness ----

# assess the distribution
hist(sp_rich$richness)

# create GLM with poisson distribution and interaction term
richness_i<- glm(richness~ duration* Temperature, data= sp_rich, family = poisson (link = log))
summary(richness_i)

# dispersion parameter
35.455/37 # = 0.9582432 < 1.5 so no overdispersion and poisson is fine

# model plots
plot(richness_i)


## Create simpler models for likelihood comparisons:
richness_no_int<- glm(richness~ duration + Temperature, data= sp_rich, family = poisson (link = log))
summary(richness_no_int)


richness_no_dur<- glm(richness~ Temperature, data= sp_rich, family = poisson (link = log))
summary(richness_no_dur)


richness_no_temp<- glm(richness~ duration, data= sp_rich, family = poisson (link = log))
summary(richness_no_temp)

#likelihood tests
anova(richness_no_dur, richness_no_int,  test="Chisq")
anova(richness_no_temp, richness_no_int,  test="Chisq")
anova(richness_no_int, richness_i,  test="Chisq")


#overall model test
anova(richness_i, test = "Chisq")

# condfidence intervals:
confint(richness_i)

# extract estimates of the model, also gives conf intervals
tidy(richness_i, conf.int = TRUE, conf.level = 0.95, exponentiate = FALSE)

#----

#----

### Plot species richness with geom_ribbon and 95% CI  ----

## Create predictions from the model:

#create data drame
predict.data.rich <- expand.grid(
  duration = seq(min(sp_rich$duration), max(sp_rich$duration), length.out = 100),
  Temperature = unique(sp_rich$Temperature)
)

#View(predict.data.rich)

# predict based on the glm poisson model
pred.rich <- predict(richness_i, predict.data.rich, type = "link", se.fit = TRUE)


#calculate the confidence intervals (still on the log scale)
lwr = pred.rich$fit - 1.96 * pred.rich$se.fit
upr = pred.rich$fit + 1.96 * pred.rich$se.fit

# backtransform to the original scale for plotting
predict.data.rich$fit    <- exp(pred.rich$fit)                         
predict.data.rich$lwr    <- exp(lwr)
predict.data.rich$upr    <- exp(upr)

# Make the plot with predicted data and confidence intervals in geom_ribbon
richness_plot<- ggplot(sp_rich, aes(x = duration, y = richness, color = Temperature, fill = Temperature)) +
  geom_ribbon(data = predict.data.rich, aes(x = duration, ymin = lwr, ymax = upr, fill = Temperature),  alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = predict.data.rich, aes(x = duration, y = fit, color = Temperature), linewidth = 1, inherit.aes = FALSE) +
  geom_point(data = sp_rich, aes(fill = Temperature), shape = 21, stroke = 0.7,position = position_jitterdodge(), size = 2) +
  labs(
    x = "Duration (Weeks)",
    y = "Species Richness (count)"
  ) +
  scale_colour_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) + 
  scale_fill_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +    
  theme_classic() +
  theme(text = element_text(size = 15),
        legend.position = "none") +
  scale_x_continuous(breaks = 0:8)

richness_plot

#----

#----

### MULTIVARIATE ANALYSIS: SPECIES COMPOSITION---- 

### Create community data ----

# Select only the necessary columns
meta_long<- full_meta %>%
  select(`Sample ID`, `Species`, `Total read`)
#View(meta_long)

# Then pivot the data to make it wide, so that each species is in its own column. 
meta_wide<- meta_long %>% 
  pivot_wider(
    names_from = `Species`, # columns are the names from the fish families
    values_from = `Total read`, # values come from the quantity column measured earlier
    values_fn = sum,  # taking the mean of the repeated values
    values_fill = 0) 

# Remove NAs
meta_wide <- meta_wide %>% select(-`NA`)


# make sure sample ID is the row names not an actual row
meta_wide<- meta_wide %>%
  column_to_rownames(var = "Sample ID")

# Remove rows that have all 0s
meta_wide_clean <- meta_wide[rowSums(meta_wide) > 0, ]


# Make another data frame with the different categories for each Sample ID
treatments<- sp_rich %>%
  distinct(`Sample ID`, temperature, duration)

View(treatments)
# treatments including only the data that has actual results
treatments_clean <- treatments %>%
  filter(`Sample ID` %in% c("A0_2", "A1_1", "A4_2", "A4_3", "A4_4", "A8_1", "A8_2", "A8_4", "FR0_1", "FR2_1", "FR8_4"))

# ----
# ----


### CCA with interaction term ----

#create new treatments data frame with only data needed
treatments_cca<- treatments_clean %>%
  column_to_rownames(var = "Sample ID")

# make the wide data into a matrix for the cca model
meta_matrix<- as.matrix(meta_wide_clean)


# do hellinger transformation (Bizzozero)
meta_hel <- decostand(meta_matrix, method = "hellinger") 


#create the cca model with the interaction
cca_model_i <- cca(meta_hel ~ duration*temperature, data = treatments_cca)
cca_model_i

#perform a permutational anova
anova(cca_model_i, by = "term",  permutations = 999)  # tests each term: Temp, Time, and interaction
anova(cca_model_i, by = "margin",  permutations = 999)
anova(cca_model_i)

# check the eigenvalues and proportions of variance explained
summary(eigenvals(cca_model_i))
eigenvals(cca_model_i, model = c("all", "unconstrained", "constrained"),
          constrained = NULL)
#----

#----

### CCA model diagnostics ----
# assess VIF for collinearity, if greater than 10 suggests high collinearity
vif.cca(cca_model_i)
#below 10

# adjusted r2 of the model
RsquareAdj(cca_model_i)
#  0.09128

#----

#----

### PERMANOVA ----
# Don't need to do any transformations before PERMANOVA because bray-curtis is automatically calculated.

## Check for dispersion in groups:
# testing between temp conditions
betadispersion.temp <- betadisper(vegdist(meta_wide_clean, method = "bray"), treatments_clean$temperature)
permutest(betadispersion.temp)

# testing between duration conditions
betadispersion.duration <- betadisper(vegdist(meta_wide_clean, method = "bray"), treatments_clean$duration)
permutest(betadispersion.duration)

# create a term for temp and duration interactions
treatments_clean$Group <- interaction(treatments_clean$temperature, treatments_clean$duration) 
view(treatments_clean)
detadispersion.interaction <- betadisper(vegdist(meta_wide_clean, method = "bray") , treatments_clean$Group) 
permutest(detadispersion.interaction)


## Make the PERMANOVA

# with interaction term
perm.i<- adonis2(meta_wide_clean ~ duration*temperature , data = treatments_clean, method = "bray", permutations = 999, by = "terms")
summary(perm.i)

# without the interaction term
perm.b<- adonis2(meta_wide_clean ~ duration+temperature , data = treatments_clean, method = "bray", permutations = 999, by = "terms")
perm.b
#----

#----

### Plot CCA using eigenvalues----
eig_vals <- eigenvals(cca_model_i) # eigenvalues are the variance explained by each canonical axis
var_exp <- round(100 * eig_vals[1:2] / sum(eig_vals), 1)  # % variance on Axis 1 and 2

# create a data frame for the plot
scores.df<- as.data.frame(scores(cca_model_i, choices = c(1,2), display = "sites"))
scores.df$site <- rownames(scores.df)
groups<- treatments_clean
scores.df$temperature<- groups$temperature
scores.df$duration<- as.factor(groups$duration)


#next for the species scores:
species.scores <- as.data.frame(scores(cca_model_i, choices = c(1,2), display = "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)


# Find the hulls for plotting
find_hull <- function(df) df[chull(df$CCA1, df$CCA2), ]

hulls.duration <- scores.df %>% 
  group_by(duration) %>% 
  do(find_hull(.))

# DURATION CCA PLOT
duration.cca.plot<- ggplot(scores.df, aes(x = CCA1, y = CCA2, color = duration)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_polygon(data = hulls.duration, aes(fill = duration, group = duration), alpha = 0.3, color = NA) +
  geom_point(data = species.scores, aes(x = CCA1, y = CCA2), 
             shape = 17, color = "black", size = 4) +  # species points
  labs(x = paste0("CCA1 (", var_exp[1], "%)"),
       y = paste0("CCA2 (", var_exp[2], "%)"),
       color = "Storage Duration (weeks)",
       fill = "Storage Duration (weeks)") +
  scale_colour_manual(values = c(
    "0" = "#ffb2fd", "1" = "#009f81", "2" = "#ff5aaf", "4" = "#8400cd", "8" = "#00fccf" )) +
  scale_fill_manual(values = c(
    "0"= "#ffb2fd", "1" = "#009f81", "2" = "#ff5aaf", "4" = "#8400cd", "8" = "#00fccf")) +
  guides(
    fill = guide_legend(override.aes = list(fill = NA))) +
  scale_x_continuous(breaks = seq(from = -3, to = 1, by = 0.5)) +
  scale_y_continuous(breaks = seq(from = -2, to = 2, by = 0.5)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),   # axis titles
        axis.text = element_text(size = 15),    # axis numbers
        legend.title = element_text(size = 16), # legend title
        legend.text = element_text(size = 14),
        legend.position = "bottom")

duration.cca.plot

# TEMPERATURE CCA PLOT
# ensure treatment labels are correct
scores.df <- scores.df %>%
  mutate(temperature = dplyr::recode(temperature,
                                     "A" = "Ambient",
                                     "FR" = "Frozen"))
# creat hulls
hulls.temp <- scores.df %>% 
  group_by(temperature) %>% 
  do(find_hull(.))

temp.cca.plot<- ggplot(scores.df, aes(x = CCA1, y = CCA2, color = temperature)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_polygon(data = hulls.temp, aes(fill = temperature, group = temperature), alpha = 0.3, color = NA) +
  geom_point(data = species.scores, aes(x = CCA1, y = CCA2), 
             shape = 17, color = "black", size = 4) +  # species points
  labs(x = paste0("CCA1 (", var_exp[1], "%)"),
       y = paste0("CCA2 (", var_exp[2], "%)"),
       color = "Storage Temperature",
       fill = "Storage Temperature") +
  scale_colour_manual(values = c(
    "Ambient" = "#E69F00", "Frozen" = "#0072B2" )) +
  scale_fill_manual(values = c(
    "Ambient" = "#E69F00", "Frozen" = "#0072B2" )) +
  guides(
    fill = guide_legend(override.aes = list(fill = NA))) +
  scale_x_continuous(breaks = seq(from = -3, to = 1, by = 0.5)) +
  scale_y_continuous(breaks = seq(from = -2, to = 2, by = 0.5)) +
  theme_classic()  +
  theme(axis.title = element_text(size = 18),   # axis titles
        axis.text = element_text(size = 15),    # axis numbers
        legend.title = element_text(size = 16), # legend title
        legend.text = element_text(size = 14),
        legend.position = "bottom")

temp.cca.plot

# plot together in a panel
duration.cca.plot + temp.cca.plot

# ----

# ----
### Heat maps with total hellinger transformation ---- 

# create new data frame for community comp
hellinger_all<- (meta_wide)

# Add metadata back (e.g., temperature, duration)
hellinger_all$temperature <- treatments$temperature
hellinger_all$duration <- treatments$duration

hellinger_all <- hellinger_all %>%
  mutate(temperature = dplyr::recode(temperature,
                                     "A" = "Ambient",
                                     "FR" = "Frozen"))

hellinger_all <- hellinger_all %>%
  group_by(temperature, duration) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

# do hellinger transformation on the sequencing reads
hellinger_all[, -(1:2)] <- decostand(hellinger_all[, -(1:2)], method = "hellinger")
View(hellinger_all)

# Split by temperature group
hell_ambient <- hellinger_all[hellinger_all$temperature == "Ambient", ]
hell_frozen  <- hellinger_all[hellinger_all$temperature == "Frozen",  ]

#----

#----

### Heat Map for frozen samples ----
hell_frozen <- hell_frozen[ , -1]

# set first column as row names
rownames(hell_frozen) <- hell_frozen[[1]] 

# create matrix
frozen_matrix<- as.matrix(hell_frozen)

frozen_matrix <- frozen_matrix[ , -1]


fr_hel_matrix_na <- frozen_matrix # create new matrix for the NAs
fr_hel_matrix_na[fr_hel_matrix_na == 0] <- NA # record 0s as NAs


# Add it to the original data frame
fr_hel_matrix_na <- rbind(fr_hel_matrix_na, new_row)
fr_hel_matrix_na <- fr_hel_matrix_na[!rownames(fr_hel_matrix_na) %in% "temperature", ]
fr_hel_matrix_na<- t(fr_hel_matrix_na)
View(fr_hel_matrix_na)

my_colors <- colorRampPalette(brewer.pal(9, "Oranges"))(100) #select the colour ramp

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
hell_ambient <- hell_ambient[ , -1]
rownames(hell_ambient) <- hell_ambient[[1]] # set first column as row names
ambient_matrix<- as.matrix(hell_ambient)
ambient_matrix <- ambient_matrix[ , -1]


a_hel_matrix_na <- ambient_matrix # create new matrix for the NAs
a_hel_matrix_na[a_hel_matrix_na == 0] <- NA # record 0s as NAs
a_hel_matrix_na<- t(a_hel_matrix_na)
View(a_hel_matrix_na)

p_a<- pheatmap(a_hel_matrix_na,
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

#----

#----

### SAC with accumcomp ----

# make row names the first column for treatments, and make sure you have a column with read counts
treatments_sac<- treatments %>%
  column_to_rownames(var = "Sample ID")


# For the duration treatment
Accum.dur <- accumcomp(meta_wide, y=treatments_sac, factor='duration', 
                       method='exact', conditioned=FALSE, plotit=FALSE)

accum.long.dur <- accumcomp.long(Accum.dur, ci=0.95, label.freq=5)
View(Accum.dur)

duration_sac_plot <- 
  ggplot(data = accum.long.dur, aes(x = Sites, y = Richness)) + 
  geom_ribbon(aes(ymin = LWR, ymax = UPR, fill = Grouping), alpha = 0.1, colour = NA) +
  geom_line(aes(colour = Grouping), size = 0.8) +
  scale_colour_manual(values = c("0" = "#ffb2fd", "1" = "#009f81", "2" = "#ff5aaf", "4" = "#8400cd", "8" = "#00fccf")) +  # line color
  scale_fill_manual(values = c("0" = "#ffb2fd", "1" = "#009f81", "2" = "#ff5aaf", "4" = "#8400cd", "8" = "#00fccf")) +    # ribbon fill
  labs(x = "Samples", y = "Species Richness", colour = "Duration", fill = "Duration") +
  theme_classic()

duration_sac_plot


# For the temperature treatment
treatments_sac <- treatments_sac %>%
  mutate(temperature = dplyr::recode(temperature,
                                     "A" = "Ambient",
                                     "FR" = "Frozen"))
Accum.temp <- accumcomp(meta_wide, y=treatments_sac, factor='temperature', 
                        method='exact', conditioned=FALSE, plotit=FALSE)

accum.long.temp <- accumcomp.long(Accum.temp, ci=0.95, label.freq=5)

temp_sac_plot <- 
  ggplot(data = accum.long.temp, aes(x = Sites, y = Richness)) + 
  geom_ribbon(aes(ymin = LWR, ymax = UPR, fill = Grouping), alpha = 0.1, colour = NA) +
  geom_line(aes(colour = Grouping), size = 1.2) +
  scale_colour_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +  # line color
  scale_fill_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +    # ribbon fill
  labs(x = "Samples", y = "Species Richness (count)", colour = "Temperature", fill = "Temperature") +
  theme_classic() +
  theme(text = element_text(size = 15),
        legend.position = "none")

temp_sac_plot

# plot together
richness_plot + temp_sac_plot
#----

#----

### Chao estimators ----
# make new data frame
treatments.chao<- treatments
treatments.chao <- treatments.chao %>%
  mutate(temperature = dplyr::recode(temperature,
                                     "A" = "Ambient",
                                     "FR" = "Frozen"))

# create interaction term between treatments
treatments.chao$Group <- interaction(treatments.chao$temperature, treatments.chao$duration) 
treatments.chao<- treatments.chao %>%
  column_to_rownames(var = "Sample ID")

# estimate chao richness for the interactions treatments
chao2_est <-specpool(meta_wide, treatments.chao$Group)
t(chao2_est)

#summarize sp richness for each treatment
summary_richness<- meta %>%
  group_by(temperature, duration) %>%
  summarise(rich_obs = n_distinct(`Species`,na.rm = TRUE),
            SD_obs = sd(rich_obs, na.rm = TRUE),
            Count = n(),
            se_obs = SD_obs/sqrt(4)) %>%
  ungroup()

# create interaction group 
summary_richness$Group <- interaction(summary_richness$temperature, summary_richness$duration) 

chao_df <- as.data.frame(chao2_est)
chao_df$Group <- rownames(chao_df)
View(chao_df)

# merge chao and observed richness into one data set
chao_richness<-  merge(summary_richness, chao_df, by = "Group", all.x = TRUE)

# categorize by richness type
richness_long <- chao_richness %>%
  pivot_longer(cols = c(rich_obs, chao),
               names_to = "RichnessType",
               values_to = "Richness")
richness_long$Richness[is.na(richness_long$Richness)] <- 0


# plot chao vs observed for each temp over time.
ggplot(richness_long, aes(x = duration, y = Richness, color = temperature, linetype = RichnessType)) +
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  # Add ribbons for Chao SE
  geom_ribbon(data = richness_long %>% filter(RichnessType == "chao"),
              aes(ymin = Richness - chao.se, ymax = Richness + chao.se, fill = temperature),
              alpha = 0.2,
              color = NA) +
  labs(x = "Duration (weeks)",
       y = "Species Richness", ) +
  theme_classic() +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  scale_fill_manual(values = c("#E69F00", "#0072B2")) +
  theme(text = element_text(size = 15),
        legend.position = "none")

# ----
# ----
### Extra data exploration ----
# Summary table for the number of Reads, bp lenghts, for each treatment (10 rows total)
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


# Treatment level summary stats
summary_meta_sd <-summary_meta %>%
  filter(!Total_Read %in% c("0"))

View(summary_meta_sd)
# 7 of the 10 treatments had detections


mean(summary_meta_sd$Total_Read)
# 97766
sd(summary_meta_sd$Total_Read)
# 193611

sd(summary_meta_sd$Total_Read)/(sqrt(7))
# 73178 = the standard error of the mean across all treatment reads

sum(summary_meta_sd$BP_length)/18

# How many reads from frozen samples?
summary_frozen_reads<- summary_meta %>%
  filter(temperature == "Frozen")
View(summary_frozen_reads)
sum(summary_frozen_reads$Total_Read)
#587927

# How many reads from ambient samples?
summary_ambient_reads<- summary_meta %>%
  filter(temperature == "Ambient")
sum(summary_ambient_reads$Total_Read)
#96438


## How many species did we detect in total?
length(unique(meta$Species))
# 10 unique species [remove the NA]
unique(meta$Species)

# how many total detections?
length(unique(meta$`Total read`))
#18 total detections

#13 samples had detections


## How many species detected for each temperature?

#frozen samples subset of df
meta_frozen_long<- meta %>%
  filter(temperature == "Frozen")
View(meta_frozen_long)
unique(meta_frozen_long$Species)
# 4 species detected from frozen samples

length(unique(meta_frozen_long$`Total read`))
# 5 total detectionsan

# ambient samples subset of meta df
meta_ambient_long<- meta %>%
  filter(temperature == "Ambient")
unique(meta_ambient_long$Species)
# 10 species

length(unique(meta_ambient_long$`Total read`))
# 14 total detections 

# percent of detections contributed by ambient samples
14/18
# 77.8% 

## Summary of unfiltered results (including all blanks, controls, and extra species) 
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

## Summary of filtered data for each sample
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
# ----

# ----


### Citations----

cite_packages()
