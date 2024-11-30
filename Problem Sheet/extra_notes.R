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

