# if we want to use height, need to convert it into cm 
height_to_cm <- function(height) {
  sapply(height, function(h) {
    if (grepl("cm", h)) { # If the value is already in cm convert it to numeric format
      as.numeric(gsub("cm", "", h))
    } else if (grepl("m",h)) {
      m<- as.numeric(gsub("m.*", "", h))
      (m*100)
    } # convert the values that are in m
    else if (grepl("ft", h)) {
      # If the value is in feet and inches
      ft <- as.numeric(gsub("ft.*", "", h)) # identify ft
      in_val <- as.numeric(gsub(".*ft |in", "", h)) # identify in
      (ft * 30.48) + (in_val * 2.54) # Convert ft and in to cm
    } else {
      NA 
    }
  })
}

data$new.height<- height_to_cm(data$Height)

data$age<- 2024 -  data$Year.of.birth # age of individual
data$siblings<- ifelse(data$Brothers > 0 | data$Sisters > 0, "Yes", "No") # does the individual have siblings?
data$n.siblings<- as.numeric(data$Brothers + data$Sisters) #how many siblings?
data <- data[data$Gender != "Other",]
View(data)


newests<- lm(hours.midnight~mc.siblings*Breakfast, data=data, na.values=T)
plot(data$hours.midnight~data$mc.siblings)

hist(data$hours.midnight)
qqnorm(resid(newest))
qqline(resid(newest))
# Residuals look normal
# Variances don't demonstrate any clear patterns and look equally distributed


# Unused Models ----
- Creating a model with Breakfast as an interaction term with time of wake up.

m.i<- glm(Exercise~Hours.Slept*Breakfast + avg.commute, data = data, family = poisson)
summary(m.i)
#AIC = 378.45 and the interaction term is not significant
# No overdispersion, Residual deviance < Residual Degrees of Freedom



anova(m.i,test="Chisq")
# Avg commute is the least significant term, so in the next model can remove it.



m.i.2<- glm(Exercise~Hours.Slept*Breakfast , data = data, family = poisson)
summary(m.i.2)
# AIC = 376.55
# No overdispersion, Residual deviance < Degrees of Freedom



m<- glm(Exercise~Hours.Slept+Breakfast , data = data, family = poisson)
summary(m)
#AIC = 376.23
anova(m, test= "Chisq")


m2<- glm(Exercise~Breakfast , data = data, family = poisson)
summary(m2) 
#AIC = 375.57
anova(m2, test= "Chisq")
#breakfast is more significant when hours slept is still included in the model. 
#Therefore we will use m as our final model.



b0 <- m.s.i$coefficients[1] # intercept
b0
b1 <- m.s.i$coefficients[2] # hours slept
b2 <- m.s.i$coefficients[3] # breakfast
b3 <- m.s.i$coefficients[4] # Quadratic term
b4 <- m.s.i$coefficients[5] # Interaction term
predicted.model <- predict(m.s.i, type='response') 
predicted.modelY <- predicted.model[data$Breakfast=="Yes"] 
predicted.modelN <- predicted.model[data$Breakfast=="No"]
predicted.modelY

logit.m.predicted <- b0 + b1*data$Hours.Slept
predicted.2 <- 1/(1+exp(-logit.psr.predicted))

plot(m.s.i)

new_data_predictions <-  data.frame(Hours.Slept = 8, Breakfast = "No")
predicted.value <- predict(m.s.i, newdata = new_data_predictions, type = "response")
predicted.value


anova(m.s.i, test ="Chisq")

summary(m.s)
