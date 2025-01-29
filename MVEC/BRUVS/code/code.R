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
maxN2<- d_maxN %>%
  group_by(code, habitat) %>%
  summarize(max.n = sum(maxN))

View(maxN2)
sample_size <- table(d_maxN$habitat)
print(sample_size)
## Relative abundance plot ----
plot2<- 
  ggplot(d_maxN, aes(x = habitat, y = maxN, fill= habitat)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(colour = habitat),shape = 21, colour = "black", alpha = 0.6)+
  scale_color_manual(values = c("Reef" = "forestgreen", "Sand" = "brown", "Seagrass" = "skyblue"))+
  scale_fill_manual(values = c("Reef" = "forestgreen", "Sand" = "brown", "Seagrass" = "skyblue")) +
  labs(
    x = "Habitat type",
    y = "Relative Abundance (MaxN)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  ) +
  ylim(0,25)


plot2

## Relative abundance analysis ----

#glm
lm_total_abundance<- glm(maxN~habitat, data = d_maxN, family = quasipoisson)
lm_bare<- update(lm_total_abundance, .~. -habitat)

summary(lm_total_abundance)
anova( lm_bare,lm_total_abundance, test = 'F')
drop1(lm_total_abundance)
summary
install.packages("emmeans")
library(emmeans)

# all post-hoc comparisons:
pairs(emmeans(lm_total_abundance, ~ habitat))

#non parametric
kruskal.test(maxN ~ habitat, data = d_maxN)
# x2 = 3.4747, df = 2, p = 0.176

plot(lm_total_abundance)
# not significant p = 0.2719, f = 1.312, df = 2,181
## Calculate richness ----
d_sp_richness1<- d %>%
  group_by(code, habitat) %>%
  summarise(sp_richness = length(unique(species_common)))

d_sp_richness<- d_sp_richness1%>%
  group_by(habitat) %>%
  summarise(n=n(), mean = mean(sp_richness), sd = sd(sp_richness), se=sd/sqrt(n))

View(d_sp_richness)

## Species richness plot ----
plot1<- 
  ggplot(d_sp_richness, aes(x = habitat, y = mean, colour= habitat)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = 0.2, size = 0.8) +
  scale_color_manual(values = c("Reef" = "forestgreen", "Sand" = "brown", "Seagrass" = "skyblue")) +
  labs(
    x = "Habitat type",
    y = "Species richness (Mean +/- SE)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
    ) +
  ylim(0,10)
  

plot1

sample_size <- table(d_maxN$habitat)
print(sample_size)

total_maxN_per_video <- d_maxN %>%
  group_by(code, habitat) %>%
  summarise(total_maxN = sum(maxN, na.rm = TRUE))

View(total_maxN_per_video)

## Species richness analysis ----
lm_richness<- glm(sp_richness~habitat, data = d_sp_richness1, family = poisson)
summary(lm_richness)
lmr_bare<- update(lm_richness, .~. -habitat)
anova( lmr_bare,lm_richness, test = 'Chisq')
drop1(lm_total_abundance)
summary
install.packages("emmeans")
library(emmeans)

# all post-hoc comparisons:
pairs(emmeans(lm_richness, ~ habitat))
lm_total_richness<- aov(sp_richness~ habitat, data = d_sp_richness1)
plot(lm_total_richness)

hist(d_sp_richness1$sp_richness)
summary(lm_total_richness)
# p < 0.01, f = 13.77, df = 2 and 27

TukeyHSD(lm_total_richness)
# reef has significantly higher richness than both seagrass and sand. Seagrass and sand are not significantly different
#             diff       lwr       upr     p adj
#Sand-Reef     -4.2 -6.464394 -1.935606 0.0002566
#Seagrass-Reef -4.1 -6.364394 -1.835606 0.0003433
#Seagrass-Sand  0.1 -2.164394  2.364394 0.9934135


#non para
install.packages("dunn.test")
library(dunn.test)

kruskal.test(sp_richness~ habitat, data = d_sp_richness1)
# p = 0.00053, df = 2, x2 = 15.061
summary(dunn.test(d_sp_richness1$sp_richness, d_sp_richness1$habitat, method = "bonferroni"))
#Col Mean-|
 # Row Mean |       Reef   |    Sand
#---------+----------------------
 # Sand |   3.423437
#            0.0009*
  #Seagrass |   3.294736  | -0.128700
             #   0.0015*  |   1.0000
TukeyHSD()

## Cod species richness----
cod_richness1<- d %>%
  filter(commercial_group== "Cod likes") %>%
  group_by(code, habitat) %>%
  summarise(sp_richness = length(unique(species_common)))

View(cod_richness2)

all_sites <- d %>%
  distinct(code, habitat)

cod_richness2<- all_sites %>%
  left_join(cod_richness1, by=c("code", "habitat")) %>%
  mutate(sp_richness = replace_na(sp_richness, 0))
  

cod_richness<- cod_richness2%>%
  group_by(habitat) %>%
  summarise(n=n(), mean = mean(sp_richness), sd = sd(sp_richness), se=sd/sqrt(n))

View(cod_richness2)


## Cod species richness plot ----
plot3<- 
  ggplot(cod_richness, aes(x = habitat, y = mean, colour= habitat)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = 0.2, size = 0.8) +
  scale_color_manual(values = c("Reef" = "forestgreen", "Sand" = "brown", "Seagrass" = "skyblue")) +
  labs(
    x = "Habitat type",
    y = "Cod-like species richness (Mean +/- SE)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  )
plot3

## Cod richness analysis ----
# glm
hist(cod_richness2$sp_richness)
lm_cod_richness<- glm(sp_richness~habitat, data = cod_richness2, family = poisson)
summary(lm_cod_richness)
lmcr_bare<- update(lm_cod_richness, .~. -habitat)
anova( lmcr_bare,lm_cod_richness, test = 'Chisq')
drop1(lm_total_abundance)
summary
install.packages("emmeans")
library(emmeans)

# all post-hoc comparisons:
pairs(emmeans(lm_richness, ~ habitat)

View(cod_richness1)
lm_cod_richness<- lm(sp_richness~ habitat, data = cod_richness2)
plot(lm_cod_richness)
summary(lm_cod_richness)
# Not significant p = 0.2464, f = 1.54, df = 2, 15

# kruskal
kruskal.test(sp_richness~habitat, data = cod_richness2)
#p = 0.04, x2 = 6.179, df = 2

dunn.test(cod_richness2$sp_richness, cod_richness2$habitat, method = "bonferroni")

sample_size <- table(cod_richness1$habitat)
print(sample_size)

## cod relative abundance ----
cod_maxN<- d %>%
  filter(commercial_group=="Cod likes") %>%
  group_by(code, habitat, species_common) %>%
  summarise(maxN = max(max_n))
View(cod_maxN)
all_sites <- d %>%
  distinct(code, habitat)

cod_maxN2<- all_sites %>%
  left_join(cod_maxN, by=c("code", "habitat")) %>%
  mutate(maxN = replace_na(maxN, 0))

cod_maxN2 <- cod_maxN2 %>%
  mutate(species_common = ifelse(is.na(species_common), "cod", species_common))

View(cod_maxN2)
View(cod_maxN)
## Cod relative abundance analysis ----
#glm
lm_cod_abundance<- glm(maxN~habitat, data = cod_maxN, family = poisson)
summary(lm_cod_abundance)
lmca_bare<- update(lm_cod_abundance, .~. -habitat)
anova( lmca_bare,lm_cod_abundance, test = 'Chisq')
drop1(lm_total_abundance)
summary
install.packages("emmeans")
library(emmeans)

# all post-hoc comparisons:
pairs(emmeans(lm_richness, ~ habitat)
      

lm_cod_abundance <- aov(maxN ~ habitat, data = cod_maxN)
summary(lm_cod_abundance)
plot(lm_cod_abundance)
TukeyHSD(lm_cod_abundance)

kruskal.test(maxN ~ habitat, data = cod_maxN2)
# p = 0.06
dunn.test(cod_maxN$maxN, cod_maxN$habitat, method = "bonferroni")
# no significant difference, but reef seems higher than sand. 
# Extra code for relative abundance ----
# Calculate relative abundance grouped by species
relative_abundance_species <- data %>%
  group_by(Species) %>%
  summarise(Total_Max_N = sum(Max_N)) %>%
  mutate(Relative_Abundance = Total_Max_N / sum(Total_Max_N))

# View the result
# Calculate relative abundance grouped by species
relative_abundance_cod <- d %>%
  filter(commercial_group== "Cod likes") %>%
  group_by(habitat) %>%
  summarise(Total_Max_N = sum(max_n)) %>%
  mutate(Relative_Abundance = Total_Max_N / sum(Total_Max_N))

relative_abundance_cod
View(cod_maxN)

sample_size <- table(cod_maxN$habitat)
print(sample_size)

plot4<- 
  ggplot(cod_maxN, aes(x = habitat, y = maxN, fill= habitat)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(colour = habitat),shape = 21, colour = "black", alpha = 0.6)+
  scale_color_manual(values = c("Reef" = "forestgreen", "Sand" = "brown", "Seagrass" = "skyblue"))+
  scale_fill_manual(values = c("Reef" = "forestgreen", "Sand" = "brown", "Seagrass" = "skyblue")) +
  labs(
    x = "Habitat type",
    y = "Cod-like Species Relative Abundance (MaxN)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  ) +
  ylim(0,7)
plot4

# View the result
print(relative_abundance_cod)

## Top species per habitat ----
top_species <- d_maxN %>%
  group_by(habitat, species_common) %>%
  summarise(n=n(), avg_abundance = mean(maxN, na.rm = TRUE), sd = sd(maxN), se=sd/sqrt(n)) %>%  # Summing abundance for each species in each habitat
  arrange(habitat, desc(avg_abundance)) %>%  # Sorting by habitat and abundance (descending)
  group_by(habitat) %>%  # Grouping by habitat to select top 5 per habitat
  slice_head(n = 5) %>%
  ungroup()


View(top_species)

ggplot(top_species, aes(x = reorder(species_common, avg_abundance), y = avg_abundance, fill = habitat)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = avg_abundance - se, ymax = avg_abundance + se),  # Error bars show +/- 1 standard error
    width = 0.2,  # Adjust the width of the error bars
    color = "black"  # Color of the error bars
  ) +
  facet_wrap(~ habitat, scales = "free_x") +  # Separate plots for each habitat
  labs(
    x = "Species",
    y = "Relative Abundance (MaxN)") +
  scale_fill_manual(values = c("Reef" = "forestgreen", "Sand" = "brown", "Seagrass" = "skyblue")) +  # Color for each habitat
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  )



Voew()