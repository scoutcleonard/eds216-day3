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
  mutate(corrected_standardized_mean_dif = sqrt(((HighFaunaN + LowFaunaN)/(LowFaunaN * HighFaunaN)) + (g^2 / (2 * (HighFaunaN + LowFaunaN))))) %>%
  mutate(weighting_term = 1 / (corrected_standardized_mean_dif^2)) %>%
  mutate(g_weighted = g * weighting_term) %>% 
  mutate(pooled_fixed_effect = sum(g_weighted) / sum(weighting_term))

#metfor does this work much more quickly!!
#escalc calculates effect sizes 
Datrich_3 <- escalc(n1i = HighFaunaN, n2i = LowFaunaN, m1i = MeanvalueatHighFauna, m2i = MeanvalueatLowFauna, sd1i = SDatHighFauna, sd2i = SDatLowFauna, data = Datrich, measure = "SMD", append = TRUE) #yi gives g
rich.overall <- rma(yi, vi, method = "FE", data = Datrich_3) #rma gives a bunch of regression modelling stats with NAs. We specify fixed effects which means certain regression modelling stats do not get filled, but they could if I had chosen another method. 

#print a forest plot of the standardized mean diffs across studies 
forest(rich.overall)

#print a funnel plot of the inverse standard errors 
funnel(rich.overall, yaxis = "seinv", main = "Inverse Standard Error")
