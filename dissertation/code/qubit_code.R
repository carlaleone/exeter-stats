# QUBIT Assays Dissertation
# 12 May 2025
# Carla Leone

### Import packages and setwd() ----
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, readr, vegan, report, BiodiversityR, RColorBrewer, car, pheatmap, broom, patchwork)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")
#----
#----
### Load and Clean the data ----
conc<- read_excel('data/new_qubit.xlsx')

# make concentration numeric to expose other NAs - expect warning of NAs introduced by coercion
conc$concentration<- as.numeric(conc$concentration)

# create new data frame for NAs - can't be used in analysis because no known level of certainty
na.conc<- conc %>% 
  filter(is.na(concentration)) %>%
  mutate(concentration = -0.01) 
#yield give a pseudo number just for visualization = -.01) #yield give a pseudo number just for visualization


#remove blanks from NA data 
na.conc<- na.conc %>%
  filter(treatment != "Blank")

# remove NAs from the original data frame
conc<- conc[!is.na(conc$concentration), ]


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
### GLM  ----
library(lme4)
hist(conc$concentration)
# glm poisson

#make sure duration is numeric
conc$duration<- as.numeric(conc$duration)


# glm with poisson because right skew
conc.model1<- glm(concentration~ duration*Treatment, data= conc, family = poisson (link = log))
summary(conc.model1)
# overdispersed - dispersion = 1.88 which is greater that 1.5

# glm quasipoisson with interaction term, 
conc.model2<- glm(concentration~ duration*treatment, data= conc, family = quasipoisson (link = log))
summary(conc.model2)
plot(conc.model2)

# check leverage of the points, any value over 2 is a concern.
hats <- as.data.frame(hatvalues(conc.model2))
hats
plot(hatvalues(conc.model2), type = 'h')
# higher leverage points in descending order = A8.4, FR4.1, FR0.1

confint(conc.model2)
tidy(conc.model2, conf.int = TRUE, conf.level = 0.95, exponentiate = FALSE)


# glm with quasipoisson to mitigate overdispersion and without interaction (simple model)
conc.model3<- glm(concentration~ duration + treatment, data= conc, family = quasipoisson (link = log))
summary(conc.model3)


# test the significance of the interaction term with likelihood ratio test
anova(conc.model3, conc.model2, test = "F")
# p = 0.006, F = 8.9462, df = 2, 21
# having the interaction makes a significant difference -> keep the interaction term.

# model without duration
conc.nodur<- glm(concentration~ treatment, data= conc, family = quasipoisson (link = log))

#interaction effect
anova(conc.nodur, conc.mode2, test="F")

plot(conc.model2)
#----
#----
# Plot GLM with 95% CI ----

predict.data <- expand.grid(
  duration = seq(min(conc$duration), max(conc$duration), length.out = 100),
  treatment = unique(conc$treatment)
)



# predict based on the glm quasi poisson model
pred <- predict(conc.model4, newdat, type = "link", se.fit = TRUE)
head(pred)

#calculate the confidence intervals (still on the log scale)
lwr = pred$fit - 1.96 * pred$se.fit
upr = pred$fit + 1.96 * pred$se.fit

# backtransform to the original scale for plotting
predict.data$fit    <- exp(pred$fit)                         
predict.data$lwr    <- exp(lwr)
predict.data$upr    <- exp(upr)


# make sure duration is numeric before plotting
na.conc$duration<- as.numeric(na.conc$duration)

# make the plot
ggplot(conc, aes(x = duration, y = concentration, color = treatment, fill = treatment)) +
  # Ribbon for 95% CI
  geom_ribbon(data = predict.data,  aes(x = duration, ymin = lwr, ymax = upr, fill = treatment), alpha = 0.2,inherit.aes = FALSE) +
  geom_line( data = predict.data,aes(x = duration, y = fit, color = treatment),size = 1,inherit.aes = FALSE) +
  geom_point(data = conc,aes(fill = treatment), shape = 21, stroke = 0.7, position = position_jitterdodge(), size = 2) +
  geom_point(data = na.conc, aes(shape = `Below Detection Limit`, color = treatment), position = position_jitterdodge(), size = 4.5, stroke = 0.7) +  
  scale_shape_manual(values = c("< 0.05 ng/µL" = 4)) +
  labs(x = "Duration (Weeks)", 
       y = "Concentration (ng/µL)") +
  scale_colour_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) + 
  scale_fill_manual(values = c("Ambient" = "#E69F00", "Frozen" = "#0072B2")) +    
  theme_classic() +
  theme(text = element_text(size = 15),
        legend.position = "none") +
  scale_x_continuous(breaks = 0:8)

