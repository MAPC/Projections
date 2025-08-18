library(tidyverse)
library(data.table)

pumas10 <- read.csv("S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/PUMA_2010Geog.csv") %>% 
  select(name_t, all_of(mpos$mpo)) %>% 
  rename(PUMA = name_t) %>% 
  mutate(PUMA = as.character(PUMA),
         PUMA = str_pad(PUMA, 5, side = "left", pad = "0")) %>% 
  pivot_longer(cols = all_of(mpos$mpo), names_to = "RPA", values_to = "in_RPA") %>%
  filter(in_RPA == 1) %>%
  select(-in_RPA) #Assigns a RPA to each PUMA
fwrite(pumas10,'S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Input_Files/v2/PUMA10_RPA_crosswalk.csv')