# QUBIT Assays Dissertation
# 12 May 2025
# Carla Leone

### Import packages and setwd() ----
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable, readr, paletteer)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
#----
#----
### Load and Clean the data ----
conc<- read_excel('data/new_qubit.xlsx')
View(conc)


# make concentration numeric to expose other NAs - expect warning of NAs introduced by coercion
conc$concentration<- as.numeric(conc$concentration)
View(conc)

# create new data frame for NAs - can't be used in analysis because no known level of certainty
na.conc<- conc %>% 
  filter(is.na(concentration)) %>%
  mutate(concentration = 0.01) 
#yield give a pseudo number just for visualization = 0.1) #yield give a pseudo number just for visualization

View(na.conc)

#remove blanks from NA data 
na.conc<- na.conc %>%
  filter(treatment != "Blank")

# remove NAs from the original data frame
conc<- conc[!is.na(conc$concentration), ]
conc

#----
#----
### Summarizing + exploring the data set  ----
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
### Model ----
library(lme4)
hist(conc$concentration)
conc

#make sure duration is numeric
conc$duration<- as.numeric(conc$duration)
View(conc)

# Start with linear model and check fit.
conc.model1<- lm(log(concentration)~duration*Treatment, data = conc)
summary(conc.model1)
plot(conc.model1)
# ok, but not great diagnostic plots

# glm with poisson because right skew
conc.model2<- glm(concentration~ duration*Treatment, data= conc, family = poisson (link = log))
summary(conc.model2)
# overdispersed - dispersion = 1.88 which is greater that 1.5


# glm with quasipoisson to mitigate overdispersion and without interaction (simple model)
conc.model3<- glm(concentration~ duration + treatment, data= conc, family = quasipoisson (link = log))
summary(conc.model3)

# glm quasipoisson with interaction term
conc.model4<- glm(concentration~ duration*treatment, data= conc, family = quasipoisson (link = log))
summary(conc.model4)
plot(conc.model4)



# test the significance of the interaction term 
anova(conc.model3, conc.model4, test = "F")
# p = 0.006, F = 8.9462, df = 2, 21
# having the interaction makes a significant difference -> keep the interaction term.

#interaction effect
drop1(conc.model4, test="F")


# individual treatment effects
summary(conc.model3)
drop1(conc.model3, test = "F")

#----
#----
### GLM Quaispoisson model diagnostics ----
conc$resid <- residuals(conc.model4, type = "pearson")
conc$fitted <- fitted(conc.model4)

#plot normal qq plot
normal_qq<- ggplot(conc, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  ggtitle("Normal Q-Q Plot")

resid_fitted_plot <- ggplot(conc, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(x = "Fitted values", y = "Pearson Residuals", title = "Residuals vs Fitted")

cooks <- cooks.distance(conc.model4)

cooks_plot <- ggplot(data = data.frame(obs = 1:length(cooks), cooks = cooks), aes(x = obs, y = cooks)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 4/length(cooks), linetype = "dashed", color = "red") +
  theme_classic() +
  labs(x = "Observation", y = "Cook's distance", title = "Influence (Cook's Distance)")

# ----
# ----
### Plot the GLM ----
duo.colours<- palette.colors(2) 
# give NA conc a character so it can be plotted 
na.conc$`Below Detection Limit` <- "< 0.05 ng/µL"

conc.plot<- ggplot(conc, aes(x = duration, y = concentration, color = treatment,fill = treatment, group = treatment)) +
  geom_smooth(method= glm, method.args = list(family = quasipoisson(link = "log")), alpha = 0.2) +
  geom_point(data = conc, aes(fill = treatment), shape = 21,color = "black", stroke = 0.7, position = position_jitterdodge(), size = 2) +
  geom_point(data = na.conc,
             aes(shape = `Below Detection Limit`, color = treatment),
             position = position_jitterdodge(), size = 4.5, stroke = 0.7) +  # X points
  scale_shape_manual(values = c("< 0.05 ng/µL" = 4)) +
  labs(
    x = "Time (Weeks of storage)",
    y = "Concentration (ng/µL)"
  ) + # Use in a ggplot2 chart:
  scale_colour_manual(name = "Treatment", values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) + 
  scale_fill_manual(name = "Treatment", values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +    
  theme_classic() +
  theme(text = element_text(size = 15))
conc.plot
#----
#----
### Checking extra models ----
# Log link (default for quasipoisson)
model_log <- glm(concentration ~ duration * Treatment, data = conc, family = quasipoisson(link = "log"))

# Identity link
model_identity <- glm(concentration ~ duration * Treatment, data = conc, family = quasipoisson(link = "identity"))

# Sqrt link
model_sqrt <- glm(concentration ~ duration * Treatment, data = conc, family = quasipoisson(link = "sqrt"))

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
plot_residuals(conc.model4, "Log Link")

#----

