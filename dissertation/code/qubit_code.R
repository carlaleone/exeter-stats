# QUBIT Assays Dissertation
# 12 May 2025
# Carla Leone

### Import the data and packages ----
library(readxl)
library(tidyverse)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
qubit <- read_excel("data/qubit_data.xls")
View(qubit)

### Clean the data ----
# make columns either factor or numeric
qubit$`Sample ID`<- as.character(qubit$`Sample ID`)
qubit$Time<- as.factor(qubit$Time)
qubit$Temperature<- as.factor(qubit$Temperature)
qubit$`Qubit read concentration (ng/¬µL)`<- as.numeric(qubit$`Qubit read concentration (ng/¬µL)`)
qubit$`Concentration (ng/¬µL)`<- as.numeric(qubit$`Concentration (ng/¬µL)`)

# remove NAs
qubit <- qubit[!is.na(qubit$`Sample ID`), ]

# making the NAs = 0 so that no data goes missing, assuming the NAs ar < 0.05 concentration
qubit <- qubit %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

# rename the columns
qubit$concentration <- qubit$`Concentration (ng/¬µL)`

qubit$duration <- as.numeric(qubit$`EventID/duration`)

# summary table
summary_table <- qubit %>%
  group_by(Time, Treatment) %>%
  summarize(
    Mean_Read = mean(concentration, na.rm = TRUE),
    SD_Read = sd(concentration, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  )

summary_table <- summary_table[!is.na(summary_table$SD_Read), ]
View(summary_table)

#adjust the times
time_labels <- c("1" = 0, "2" = 1, "3" = 2, "4" = 4, "5" = 8)

summary_table <- summary_table %>%
  mutate(Time = time_labels[Time])


### Make basic graph ----

summary_table$Time<- as.numeric(summary_table$Time)

ggplot(qubit.2, aes(x = duration, y = concentration, color = Treatment,fill = Treatment, group = Treatment)) +
  geom_smooth(method= glm, alpha = 0.2) +
  geom_point() +
  labs(
    x = "Time (Weeks of storage)",
    y = "Concentration ((ng/¬µL))"
  ) +
  theme_classic()

?geom_smooth
### Models ----
library(lme4)
hist(qubit.2$concentration)

qubit.2<- qubit %>%
  filter(Treatment != "Extraction Blank")

View(qubit.2)
 
qubit.2$duration<- as.numeric(qubit.2$duration)
qubit.2$concentration <- qubit.2$concentration + 0.001

qubit.model1<- glm(concentration~ duration*Treatment, data= qubit.2, family = quasipoisson(link= "sqrt"))
summary(qubit.model1)
 


plot(qubit.model1)

### Post-hoc emmeans----
library(emmeans)

emm<- emmeans(qubit.model1, ~Treatment | duration, at = list(duration = c(0,1,2,4,8)))
summary(emm)
