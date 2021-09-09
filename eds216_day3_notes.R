# add relevant libraries 
library(tidyverse)
library(metafor)

# read in the data
Datrich <- read.csv("Gardner_et_al_data-KAR35vs02_onlyprimates_nospaces.csv")

#add a new column that includes the mean difference (between high and low fauna conditions)
Datrich_new <- Datrich %>%
  mutate(mean_diff = MeanvalueatHighFauna - MeanvalueatLowFauna)
