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
qubit$`Sample ID`<- as.character(qubit$`Sample ID`)
qubit <- qubit[!is.na(qubit$`Sample ID`), ]

