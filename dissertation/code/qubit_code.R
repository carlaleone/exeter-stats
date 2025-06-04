# QUBIT Assays Dissertation
# 12 May 2025
# Carla Leone

### Import packages ----
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable, readr, paletteer)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
#----
#----
### Load and Clean the data ----
conc<- read_excel('data/qubit_data.xls') %>%
  dplyr:: select( - 'Qubit read concentration (ng/¬µL)') %>%
  rename('duration'= 'EventID/duration', 'concentration' = 'Concentration (ng/¬µL)')
  
# remove the full NA rows
conc<- conc[!is.na(conc$`Sample ID`), ]

# make concentration numeric to expose other NAs - expect warning of NAs introduced by coercion
conc$concentration<- as.numeric(conc$concentration)
conc

# create new data frame for NAs - can't be used in analysis because no known level of certainty
na.conc<- conc %>% 
  filter(is.na(concentration)) %>%
  mutate(concentration = 0.1) 
#yield give a pseudo number just for visualization = 0.1) #yield give a pseudo number just for visualization

View(na.conc)

#remove blanks from NA data 
na.conc<- na.conc %>%
  filter(Treatment != "Extraction Blank")

# remove NAs from the original data frame
conc<- conc[!is.na(conc$concentration), ]
conc

#----
#----
### Summary  ----
# summary table for the conc data set
summary_table <- conc %>%
  group_by(Treatment, duration) %>%
  summarize(
    mean_conc = mean(concentration, na.rm = TRUE),
    SD_conc = sd(concentration, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  )

View(summary_table)

# summary for the NAs

summary_na <- na.conc %>%
  group_by(Treatment, duration) %>%
  summarize(
    Count = n())

summary_na

# More of the frozen samples had NAs
# 11 frozen NAs and 4 Ambient NAs
# 3 for week 0
# 0 for week 1
# 2 for week 2
# 3 for week 4
# 7 for week 8



#----
#----
### Make first linear graph ----
duo.colours<- palette.colors(2) 

conc.plot<- ggplot(conc, aes(x = duration, y = concentration, color = Treatment,fill = Treatment, group = Treatment)) +
  geom_smooth(method= glm, method.args = list(family = quasipoisson(link = "log")), alpha = 0.2) +
  geom_point(data = conc) +
  geom_point(data = na.conc, pch = 4, position = position_jitterdodge(), size = 1.8) +
  labs(
    x = "Time (Weeks of storage)",
    y = "Concentration (ng/µL)"
  ) + # Use in a ggplot2 chart:
  scale_colour_paletteer_d("lisa::BridgetRiley") +
scale_fill_paletteer_d("lisa::BridgetRiley") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  theme_classic() +
  theme(text = element_text(size = 15))

conc.plot 

na.conc
#----
#----
### Models ----
library(lme4)
hist(conc$concentration)
conc
 
qubit.2$duration<- as.numeric(qubit.2$duration)

conc.model1<- lm(log(concentration)~duration*Treatment, data = conc)
summary(conc.model1)
plot(conc.model1)
# ok, but not great diagnostic plots

# glm with poisson because right skew
conc.model2<- glm(concentration~ duration*Treatment, data= conc, family = poisson (link = log))
summary(conc.model2)
# overdispersed


# glm with quasipoisson to mitigate overdispersion and without interaction
conc.model3<- glm(concentration~ duration+Treatment, data= conc, family = quasipoisson (link = log))
summary(conc.model3)

# glm quasipoisson with interaction term
conc.model4<- glm(concentration~ duration*Treatment, data= conc, family = quasipoisson (link = log))
summary(conc.model4)
plot(conc.model4)

# test the significance of the interaction term 
anova(qubit.model3, qubit.model2, test = "F")
drop1(qubit.model2)
drop1(qubit.model3)
# having the interaction makes a significant difference -keep the interaction term.

#----
#----
### Checking the models ----
# Log link (default for quasipoisson)
model_log <- glm(concentration ~ duration * Treatment, data = qubit.2, family = quasipoisson(link = "log"))

# Identity link
model_identity <- glm(concentration ~ duration * Treatment, data = qubit.2, family = quasipoisson(link = "identity"))

# Sqrt link
model_sqrt <- glm(concentration ~ duration * Treatment, data = qubit.2, family = quasipoisson(link = "sqrt"))

cat("Residual Deviance (Log Link):", model_log$deviance, "\n")
cat("Residual Deviance (Identity Link):", model_identity$deviance, "\n")
cat("Residual Deviance (Sqrt Link):", model_sqrt$deviance, "\n")


# Function to plot residuals vs fitted values
plot_residuals <- function(model, model_name) {
  df <- data.frame(Fitted = fitted(model), Residuals = residuals(model, type = "pearson"))
  ggplot(df, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Residuals vs Fitted for", model_name), x = "Fitted Values", y = "Pearson Residuals")
}

# Plot residuals
plot_residuals(model_log, "Log Link")
plot_residuals(model_identity, "Identity Link")
plot_residuals(model_sqrt, "Sqrt Link")

#----
#----
### Post-hoc emmeans----
library(emmeans)

emm<- emmeans(qubit.model1, ~Treatment | duration, at = list(duration = c(0,1,2,4,8)))
summary(emm)

#----
#----
### Try a negative exponential graph + model ----
ggplot(conc, aes(x = duration, y = concentration, color = Treatment,fill = Treatment, group = Treatment)) +
  geom_smooth(method="lm", formula= (y ~ exp(x)), alpha = 0.2) +
  geom_point(data = conc) +
  geom_point(data = na.conc, pch = 4, position = position_jitterdodge(), size = 1.8) +
  labs(
    x = "Time (Weeks of storage)",
    y = "Concentration (ng/µL)"
  ) + # Use in a ggplot2 chart:
  scale_colour_paletteer_d("lisa::BridgetRiley") +
  scale_fill_paletteer_d("lisa::BridgetRiley") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  theme_classic() +
  theme(text = element_text(size = 15))

