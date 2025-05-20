# Data Camp with Molly
#using Gayatri's data for practice

### Load data and packages ----
library(pacman)
pacman::p_load(stringr, tidyverse, readxl, patchwork, flextable, readr)
getwd()
setwd("/Users/carlaleone/Desktop/Exeter/dissertation")

### Using pipelines ----
conc<- read_csv('data/Scilles24_MscDalvie_QUBIT.csv') %>%
  dplyr:: select(-'Sample ID...1', - 'Qubit read concentration (ng/µL)') %>%
  rename('SampleID' = 'Sample ID...5','EventID'= 'EventID/duration', 'conc' = 'Concentration (ng/µL)')

conc

### Joining qubit and metabarcoding data ----
data <- left_join(seq, conc, by = c('TempDur_ID', 'Temperature', 'Duration', 'Replicate'))

stopifnot(nrwo(.))==250 #set it as the number fo rows you want

### Concentration ----
#na.conc<- conc %>% filter(is.na(yield)) %>%
#  mutate(yield = 0.1) #yield give a pseudo number just for visualization


#ggpolot()+
  geom_point(data = na.conc, pch, pch = 4, position = postion_dodge)


### Modelling ----
#model predictions, 
#type = 'link' to have 
#lines and CI for the model of metabarcoding, not geom_smooth. Can use geom_smooth for the Qubit)
# remembering to look for correlations and overdisperion -> for example model of qubit used negative binomial