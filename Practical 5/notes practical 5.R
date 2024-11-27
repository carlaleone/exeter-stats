# Practical 5
# Carla Leone
# November 22


# 2 Getting started  ----
## 2.1 Importing Data
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/Practical 5")
s.data<- read.csv("faecal and pvl sperm morphology data.csv", stringsAsFactors=TRUE)
View(s.data)

## 2.2 Descriptive Stats
#check sample size:
nrow(s.data) #813
length(unique(s.data$bird.no)) #27
#number of sperm per bird per sample
table <- table(s.data$sample.type, s.data$bird.no)
table

summary(table[1,])
#faecal sperm

summary(table[2,])
#pvl sperm

#distribution of sperm traits:
par(mfrow=c(2,2))
hist(s.data$head.length, xlab="head", main=NULL)
hist(s.data$midpiece.length, xlab="midpiece", main =NULL)
hist(s.data$tail.length, xlab="tail", main=NULL)
hist(s.data$total.length, xlab="total", main =NULL)

#EXERCISE 5.1
#Looking at these distributions, what do you conclude with respect to their distribution?
# head, tail, and midpiece generally seem normally distributed, however, in total length
# there is a slight skew to the right in the data. 

#are the traits correlated?:
cor(s.data[, 3:6])

#plot all at once:
pairs(cbind(s.data$head.length, s.data$midpiece.length, s.data$tail.length,
            s.data$total.length),
      labels = c("head", "midpiece", "tail", "total"))

#test significance of correlations
cor.test(s.data$head.length, s.data$midpiece.length)
cor.test(s.data$tail.length, s.data$total.length)
cor.test(s.data$tail.length, s.data$midpiece.length)
cor.test(s.data$head.length, s.data$total.length)



#EXERCISE 5.2: WHICH TRAITS SHOW STRONGEST CORRELATIONS?
# tail and total length are most correlated. This does not surprise me 
#as i would assume that tails would make up a big proportion of the total body length.

#EXERCISE 5.3: Calculate total legnth yourself and compare to toal length, do the measures mathc?
s.data$calc.total.length <- s.data$head.length + s.data$midpiece.length + s.data$tail.length
View(s.data)
# Yes, it matches the values from total length. 


# 3 Is PVL sperm different? ----

#plot tail lenfth against sample type. sample type is a factor 
par(mfrow=c(1,1))
plot(tail.length ~ sample.type, data=s.data)
#PVL sperm has slightly longer tail, but still lots of variation


## 3.1 t test
#default t test is Welch 2 sample t test, does not assume that variances in both groups are equal. 
t.test(tail.length ~ sample.type, data = s.data)


#EXERCISE 5.4 Use lm() to test for a difference in tail.length between pvl and faecal sperm. How do they compare?
summary(lm(tail.length ~ sample.type, data = s.data))
#pvl estimate = 0.5465 
# Adjusted R squared = 0.0001906 
#df =  811
# p = 0.2829
# The p value is slightly lower and there are more degrees of freedom when using the lm. However, the intercept of faecal is the same. 

#plot sample types per male:
plot(tail.length ~ bird.no, data=s.data, pch=NA)
points(tail.length ~ bird.no, data=s.data[s.data$sample.type=="faecal", ], col="blue")
points(tail.length ~ bird.no, data=s.data[s.data$sample.type=="pvl", ], col="green")

#more variation in tail.length among males than within the ejaculate of a single
# male. between male variation could be masking differences between ovl and faecal sperm.

# Could calculate mean tail length per bird and sample type using paired t test.
mean.tail <- aggregate(s.data$tail.length, by=list(s.data$bird.no, s.data$sample.type), mean)
colnames(mean.tail) <- c("bird.no", "sample.type", "mean.tail")
t.test(mean.tail$mean.tail[mean.tail$sample.type=='pvl'],
       mean.tail$mean.tail[mean.tail$sample.type=='faecal'], paired=TRUE)

# still not significant

# EXERCISE 5.5  perform a paired t-test using t.test(), but without specifying paired=TRUE?
?t.test
s.data.2<- reshape(s.data, direction = "wide",
                             idvar = "sample.type", timevar = "tail.length")
View(s.data.2)
#Could maybe reshape the data as done in the examples, then run a normal t test
# or calculated differences of pvl and faecal for each observations first
mean_tail_length <- aggregate(tail.length ~ bird.no + sample.type, data = s.data, mean)
wide_data <- reshape(mean_tail_length,
                                 idvar = "bird.no",
                                 timevar = "sample.type",
                                 direction = "wide")
View(wide_data)
wide_data$difference<-with(wide_data,abs(tail.length.faecal - tail.length.pvl))
t.test(tail.length.faecal~tail.length.pvl, data=wide_data)

## 3.2 Linear Models
# t test reduces the number of data points

#EXERCISE 5.6 What do you think is the correct sample size for our analysis? Is it the number of males, or is
#it the number of sperm? Compare this analysis to the feeding experiment discussed during the
#lecture. What are the similarities, and what are the differences?

m <- lm(tail.length ~ sample.type + bird.no, data=s.data)
summary(m)


# EXERCISE 5.7 What does the estimate tell you?

# fit same model but with bird as factor

m <- lm(tail.length ~ sample.type + as.factor(bird.no), data=s.data)
summary(m)

# EXERCISE 5.8
# Yes, it is still significant

## 3.3 Linear Mixed Models
install.packages("lme4")
library(lme4)

mm<- lmer(tail.length ~ sample.type + (1|bird.no), data=s.data)
summary(mm)

#main difference 1 = estimate for fixed and random effects
#main difference 2 = no p value, only f value. not obvious df

#can still compare models using anova
#fit without sample type to get signficiance of samplet type in later anova.
summary(mm.red <- lmer(tail.length ~ 1 + (1|bird.no), data=s.data))

#now test 
anova(mm.red,mm)
# The tail of sperm that have reached the PVL is on average (± s.e.) 0.44 (±
#0.18) µm longer than sperm collected from faecal samples (χ2 1=5.86, p=0.016)”.


#to do it ourselves
mm.ml <- lmer(tail.length ~ sample.type + (1|bird.no), data=s.data, REML = FALSE)
mm.red.ml <- lmer(tail.length ~ 1 + (1|bird.no), data=s.data, REML = FALSE)
anova(mm.red.ml, mm.ml)

# EXERCISE 5.9
summary(mm)
summary(mm.ml)

# another method of of assessing stat significance using CIs
confint(mm)

# assumption about correct df, use `KRmodcomp()`
install.packages("pbkrtest")
library(pbkrtest)
KRmodcomp(mm.red, mm)
#df are now no longer whole numbers

#package lmertEST GIVE DF AND P VALUES
install.packages("lmerTest")
library(lmerTest)
mm <-  lmer(tail.length ~ sample.type + (1|bird.no), data=s.data)
summary(mm)
anova(mm)


## EXERCISE 5.10
s.data$head.sperm<- s.data$head.length / s.data$total.length
hist(s.data$head.sperm)
mm2 <-lmer(head.sperm ~ sample.type + (1|bird.no), data=s.data)
summary(mm2)
anova(mm2)

# 4 Is PVL less sperm variable?

#comparing coefficients of variation
#cv of x is ratio of stdev to trait mean. 

#EXERCISE 5.11
#Why do you think the authors compare coefficients of variation rather than standard deviations? Do you agree?
# Used it to assess whether variance between samples differed. It is unitless and therefore perhaps

#calculate sd
sd <- aggregate(cbind(s.data$head.length, s.data$midpiece.length,
                      s.data$tail.length, s.data$total.length),
                by=list(s.data$bird.no, s.data$sample.type), sd)
head(sd)

mean <- aggregate(cbind(s.data$head.length, s.data$midpiece.length,
                        s.data$tail.length, s.data$total.length),
                  by=list(s.data$bird.no, s.data$sample.type), mean)

cv <- sd[3:6] / mean[3:6]
cv <- cbind(sd[1:2], cv)
colnames(cv) <- c("bird.no", "sample.type", "cv.head", "cv.midpiece", "cv.tail", "cv.total")

#does cv differ between sample types?
t.test(cv$cv.tail[cv$sample.type=='pvl'], cv$cv.tail[cv$sample.type=='faecal'], paired=TRUE)
plot(cv.tail ~ sample.type, data = cv)
#reduction in variability in tail length for pvl.

## 4.2 mixed model approach

#looking at variance among males
mm.faecal <- lmer(tail.length ~ 1 + (1|bird.no), data=s.data[s.data$sample.type=="faecal", ])
summary(mm.faecal)

mm.pvl <- lmer(tail.length ~ 1 + (1|bird.no), data=s.data[s.data$sample.type=="pvl", ])
summary(mm.pvl)

aggregate(sd$V3, by=list(sd$Group.2), mean)
confint(mm.faecal, level = 0.83)

#we are comparing variance rather than coefficients of variation, but PVL still less variable than faecal sperm

