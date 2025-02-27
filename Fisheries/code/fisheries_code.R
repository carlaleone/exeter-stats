# Fisheries Management Assessment 2
# Carla Leone
# Feb 27 2025

## Import data and load packages ----
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/Fisheries")
library(tidyverse)
library(readxl)
quantity <- read_excel("Fisheries.xlsx", 
                                    sheet = "Total Quantity")
value<- read_excel("Fisheries.xlsx", 
                   sheet = "Total Quality")
View(quantity)
View(value)
