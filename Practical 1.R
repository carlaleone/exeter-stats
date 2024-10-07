### Practical 1 Stats
### Carla Leone
### 30 September 2024 

## Variables ---- 
#calculations
2+3
2-3
2/(3*2+4)
3^2 

#variables 
a <- 10
a
a+3
2-a
3^a
#change a
a<- 5
a+3
#R is case sensitive
A<- 2
a/A
#excercise pythagorean theorem 1.2
c<- sqrt(5^2 + 2^2)
c
# characters
x <- "dog"
x
y <- "I like cats"
y
x <- "7"
x*3 #error bc not numeric
x <- 1; y <- 3; x/y
#multiple commands in same line
1 + 2 + 3 +
  4 + 5
# can enter to have commands separate in lines
# other useful functions...
sqrt(9)
sqrt(5*5)
9^0.5
(5*5)^0.5

#how many characters in a variable?
a <- "rhododendron"
nchar(a)

#extract part of a string in the word (extract characters 4 to 6)...
substr(a,4,6)

#excercise 1.3 extracting last 4 characters of any a at least 4 characters long.
a<- "colours"
nchar(a)
substr(a,4,7)

## Vectors ----
mass <- c(13, 10, 9, 20, 26)
mass
# in R can perform calculations on all elements of a vector at once:
mass.kg <- mass/1000
mass.kg
# can apply functions to a vector
mean(mass)
nchar(mass)

#To refer to individual elements of a vector, you need to use []. These square brackets can be very confusing in the beginning, but you will be using them a lot, so it is worth practising how to use them:
mass[5]      # The fifth element of mass
mass[2:4]    # The second, third and fourth element of mass
mass[c(1,3)] # The first and the third element of mass

#can also remove elements
mass[-c(2,4)] # NOT the second and fourth element of mass

#can replace specific element of mass
mass[4] <- 35 # Replace the fourth element of mass with 35
mass

## Boolean Variables ----
# Boolean variables only have two different values, TRUE and FALSE. You will use these a lot when comparing variables or creating subsets of your data, although often without noticing it.
x <- 10
x == 11 #response is FALSE
x > 9 #TRUE
y <- 20
x == y #FALSE
x != y #TRUE- test whether two variables are unequal, you use !=. Note that to test if two variables are identical, you use == instead of =. The single = is reserved for specifying variables in functions

#excercise 1.4: x would then be made to be the same number as y

a<- 1:10
a #is the vector of number 1 through 10
a >= 5
pet <- c("dog", "cat", "cat", "dog")
pet == 'dog'

#subset in a vector
a[a>5]


#useful functions for vectors
t <- seq(5, 20, 3) # numbers 5 to 20, every 3rd number
t
pet <- rep('cat', 10) 
pet

#excercise 1.5
q <- seq(5,1, -2)
q
p<- rep(q,each=5)
p

## Factors ----
colour <- c("yellow", "brown", "brown", "yellow", "yellow", "yellow", 
            "brown", "yellow", "brown", "yellow")
colour
summary(colour)
colour <- factor(colour)
colour
summary(colour)
#What this has done is created two vectors, one with 1 or 2 (for brown or yellow), and a second containing the two colours. The second vector can be seen as a key that translates the numbers into colours. Factors are only useful for variables that can only have a limited number of different values.



## Testing and coercing ----
is.numeric(mass)
mass <- as.character(mass)
is.numeric(mass)
is.character(mass)
mass
is.factor(colour)

#excercise 1.6: turning character vector to numeric vector...?
as.character(13)

## Creating random numbers ----

#R has functions that can generate random numbers for all the common distributions, including the uniform (runif()) and the normal distribution (rnorm()).
#To generate normally distributed data,you need to specify the length of the vector (i.e. the sample size), as well as the two parameters that describe the normal distribution: the mean and the standard deviation. So, to randomly draw 100 values with a mean of 10 and a standard deviation of 2 (i.e. a variance of 22 = 4) you use:
a <- rnorm(100, 10, 2) 
mean(a)
sd(a)
min(a)
max(a)
#excercise 1.7: you still have random numbers, they will be similar though because of the parameters set.

hist(a)

## Missing Values ----
#missing data is common in real data sets. In R, missing values are coded with NA. Many functions will by default not accept any NA’s:
a <- rnorm(10, 1, 1)
mean(a)
a[7] <- NA
a
mean(a) #result is now NA

#to remove the NA first need to find it
a[a!="NA"] #NA is not a character value, so this does not work

is.na(a)
#to remove the missing value from a:
b <- a[is.na(a)==FALSE]
b
mean(b)
#mean also has built in function to remove NA
mean(a, na.rm = TRUE)

## if() statements ----
# Sometimes you only want to do something if a certain requirement is fulfilled. For example, you want to calculate a + 10, but only if a is numeric:
a <- 4
if (is.numeric(a)==TRUE) a <- a + 10
a
a <- "4"
if (is.numeric(a)==TRUE) a <- a + 10
a #just results in 4 because a is not numeric here

# using more than 1 line use {}
a <- 4
if (is.numeric(a)==TRUE) {
  b <- a / 3
  c <- round(b, 2)
}
c

# expand the above by specifying not only what you want to do if is.numeric(a)==TRUE, but also if this requirement is not fulfilled, so if is.numeric(a)==FALSE:
a <- "3"
if (is.numeric(a)==TRUE) {
  a / 4
} else {
  as.numeric(a)/4
  print("a was coerced to be numeric before dividing by 4!")
}


#can also use the 'ifelse()' function
a <- "cat"
ifelse(is.numeric(a)==TRUE, "a is numeric", "a is NOT numeric")

## for() loops ----

#sometimes you need to perform a certain action multiple times after each other. For example, instead of:
print("1")
print("2")
#....
for (i in 1:5) {
  print(i)
}

#other example
for (pets in c("dogs", "cats", "chickens", "spiders")) {
  my.pet <- paste("I like", pets)
  print(my.pet)
}

## Writing Function ----

# You can create our own function to calculate the mean:
my.mean <- function(x) {
  sum.x <- sum(x)
  n <- length(x)
  sum.x/n
}
z <- c(1,4,2,5,6)
my.mean(z)

q <- c(5461,534,456,3,778)
my.mean(q)

#excercise 1.8 use pythag theorem to get c for any a and b
pythag<- function (x) {
  sum.ab<- sum(x[1]^2 + x[2]^2)
  sqrt(sum.ab)
}

t<- c(3,8)
pythag(t)

## Arrays and matrices ----
# flexible way of storing data. Array can have any number of dimensions, but matrix is 2.
# empty matrix with 3 rows and 2 columns:
array(NA, dim = c(3,2))
#or
matrix(NA, nrow = 3, ncol=2)
# you can use [ and ] to select specific elements of a matrix, where you first specify the row number and then the column number (I repeat, first row, then column…):
m <- matrix(seq(1,16), nrow=4)  # Create a 4*4 matrix filled with numbers from 1 to 16
m
m[2, 4]      # second row, fourth column
m[4, 2]      # fourth row, second column
m[ , 3]      # all rows, third column
m[1:2, 3]    # first and second row, third column
m[c(2,4), 4] # second and fourth row, fourth column

# excercise 1.9 5x5 matrix with random numbers. then turn into a 25 x3 matric

m<- matrix(rpois(25,30),nrow=5)
m<- as.vector(m)
m
s<- matrix(rpois(25,30), nrow= 25, ncol = 3)
s[, 3]<- m
s

## Data Frames ----
#You can use data.frame() to create a dataframe from a set of vectors:
data <- data.frame(ID=LETTERS[1:5], mass=rnorm(5, 10, 1))
data
# Two columnes: ID and Mass: contains both numeric and character, not possible in a matrix
# By default, data.frame() will convert character vectors to factors. If you don’t want it to do this, include stringsAsFactors=FALSE:
data <- data.frame(ID=LETTERS[1:5], mass=rnorm(5, 10, 1), stringsAsFactors=FALSE)
data


## Subsetting data frames ----
# use row and column numbers to refer to elements of data frame
data[2, 2]
data[ , 1]
data[1:2, ]
#easier to refer to column by name
data$ID                  # All ID's
data[, "ID"]             # Again all ID's
data[data$mass>10, ]     # All columns, but only those rows where mass>10
data$mass[data$ID=='A']  # The mass for individual A
#subsetting a single column
# what if you wantr mass for individuals A, C and E
data$mass[data$ID=="A" | data$ID=="C" | data$ID=="E"]

#might be easier to create a vector that has IDs for which you need the weight, then use %in% to select the IDs listed in the vector
select <- c("A", "C", "E")
data$mass[data$ID %in% select]

## Creating new variables ----
#add new columns to data frame
data$mass.kg <- data$mass/1000
data

## Importing Data ----
getwd()
setwd("/Users/carlaleone/Downloads") 
worms <- read.table("worms.txt", header=TRUE)
worms
?read.table()

## Exploring Data frame ----
names(worms) #extract the names of all variables
head(worms) #first 6 lines
tail(worms) #last 6 lines
nrow(worms)
ncol(worms)
summary(worms)
str(worms)
last<- function (x){
  n<- nrow(x)
  t<- 0.1*n
  View(x[(n-t),])
}

last(worms)
?tail()

#1.13
worms$acres<-  worms$Area*2.47105
View(worms)
damp<- worms$Damp == "TRUE"
damp <- subset(worms, worms$Damp == "TRUE")
damp <- subset(damp, worms$acres> 3)
damp

## Useful Functions ----
#Sometimes you don’t want to apply a function, for example mean(), to all values for a certain variable, but to different subsets. The aggregate() function allows you to do this:
aggregate(worms$Worm.density, by=list(worms$Vegetation), mean)

## Merging data frames ----
#may have another data frame, which contains the average amount of rainfall for each field site, and it would be nice to include these data into our worms data frame. This you can do with merge():
worms.rainfall <- merge(worms, rainfall, by="Field.Name")
# does not work - challenge
nrow(worms)
fieldnames<- c(worms$Field.Name)
fieldnames
rainfall<- data.frame(nrow=20, row.names = fieldnames, check.rows = FALSE)
rainfall<- data.frame(Field.Name = c(fieldnames),
                       avg.rainfall = rnorm(20, mean= 5, sd = 0.3))
rainfall
worms.rainfall <- merge(worms, rainfall, by="Field.Name")
worms.rainfall

## Exporting data frames ----
write.table(worms, "worms_output.txt", quote = FALSE, sep = '\t', 
            row.names = FALSE, col.names = TRUE)
## Plotting ----
#histograms
hist(worms$Area)
hist(worms$Worm.density)
# plot together
par(mfrow = c(1, 2)) 
hist(worms$Area)
hist(worms$Worm.density)

#box plots
is.factor(worms$Vegetation)
is.character(worms$Vegetation)
worms$Vegetation <- as.factor(worms$Vegetation)
par(mfrow = c(1, 1))  # From now on you want 1 plot again
plot(worms$Worm.density ~ worms$Vegetation)
# or, because variables are in same data frame
plot(Worm.density ~ Vegetation, data=worms)
#boxplot info:
#The middle horizontal line shows the median worm density.
#The bottom and top of the box show the 25 and 75 percentiles respectively (i.e. the location of the middle 50% of the data), also called the interquartile range.
#The horizontal line joined to the box by the dashed line (sometimes called the “whisker”) shows either the maximum or 1.5 times the interquartile range of the data (whichever is smaller).
#Points beyond the whiskers (outliers) are drawn individually.
#Boxplots not only show the location and spread of data, but also indicate skewness (asymmetry in the sizes of the upper and lower parts of the box).


# Scatter plots
plot(worms$Worm.density ~ worms$Area)
# label axes
plot(Worm.density ~ Area, data = worms, xlab="Area (hectares)", ylab="Worm density", col = "red", pch =15)
?par
