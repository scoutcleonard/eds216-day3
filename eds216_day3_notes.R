# add relevant libraries 
library(tidyverse)
library(metafor)

# read in the data
Datrich <- read.csv("Gardner_et_al_data-KAR35vs02_onlyprimates_nospaces.csv")

#add a new column that includes the mean difference (between high and low fauna conditions)
Datrich_1 <- Datrich %>%
  mutate(mean_diff = MeanvalueatHighFauna - MeanvalueatLowFauna)

#add a new column that includes the pooled standard deviation 
Datrich_2 <- Datrich_1 %>% 
  mutate(pooled_sd = sqrt((((HighFaunaN - 1) * (SDatHighFauna^2)) + (LowFaunaN - 1) * (SDatLowFauna^2))/((HighFaunaN - 1) + (LowFaunaN - 1)))) %>% 
  mutate(mean_se = pooled_sd * sqrt((1 / HighFaunaN) + (1 / LowFaunaN))) %>%
  mutate(standarized_mean_dif = ((MeanvalueatHighFauna - MeanvalueatLowFauna)/pooled_sd)) %>% 
  mutate(g = ((standarized_mean_dif) * (1 - (3 / ((4 * (HighFaunaN + LowFaunaN - 2) - 1)))))) %>%
  mutate(corrected_standardized_mean_dif = sqrt(((HighFaunaN + LowFaunaN)/(LowFaunaN * HighFaunaN)) + (g^2 / (2 * (HighFaunaN + LowFaunaN)))))

#