# QUBIT Assays Dissertation
# 12 May 2025
# Carla Leone

### Import the data and packages ----
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable, readr)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
qubit <- read_excel("data/qubit_data.xls")
qubit
colnames(qubit)


### Clean the data ----
# adjust tibble
conc<- read_excel('data/qubit_data.xls') %>%
  dplyr:: select( - 'Qubit read concentration (ng/¬µL)') %>%
  rename('duration'= 'EventID/duration', 'concentration' = 'Concentration (ng/¬µL)')
  
conc<- conc[!is.na(conc$`Sample ID`), ]

conc$concentration<- as.numeric(conc$concentration)
conc

# remove NAs- NAs can't be used because no known level of certainty
na.conc<- conc %>% 
  filter(is.na(concentration)) %>%
  mutate(concentration = 0.1) 
#yield give a pseudo number just for visualization = 0.1) #yield give a pseudo number just for visualization

View(na.conc)

#remove blanks
na.conc<- na.conc %>%
  filter(Treatment != "Extraction Blank")


# summary table
summary_table <- qubit %>%
  group_by(Replicate, Duration) %>%
  summarize(
    Mean_Read = mean(concentration, na.rm = TRUE),
    SD_Read = sd(concentration, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  )

View(summary_table)


### Make basic graph ----

ggplot(conc, aes(x = duration, y = concentration, color = Treatment,fill = Treatment, group = Treatment)) +
  geom_smooth(method= glm, alpha = 0.2) +
  geom_point(data = na.conc, pch = 4, position = position_jitterdodge(), size = 1.2) +
  labs(
    x = "Time (Weeks of storage)",
    y = "Concentration ((ng/¬µL))"
  ) +
  theme_classic()

qubit

na.conc
### Models ----
library(lme4)
hist(qubit.2$concentration)

qubit<- qubit %>%
  filter(Treatment != "Extraction Blank")

View(qubit)
 
qubit.2$duration<- as.numeric(qubit.2$duration)

qubit.model1<- lm(log(concentration)~duration*Treatment, data = qubit.2)
summary(qubit.model1)
plot(qubit.model1)

qubit.model2<- glm(concentration~ duration*Treatment, data= qubit.2, family = quasipoisson (link = log))
summary(qubit.model2)
qubit.model3<- glm(concentration~ duration+Treatment, data= qubit.2, family = quasipoisson (link = log))
summary(qubit.model3)
# more dispersion parameter and less significance in the Freezer treatment
 
anova(qubit.model3, qubit.model2, test = "F")
drop1(qubit.model2)
drop1(qubit.model3)

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

### Post-hoc emmeans----
library(emmeans)

emm<- emmeans(qubit.model1, ~Treatment | duration, at = list(duration = c(0,1,2,4,8)))
summary(emm)
