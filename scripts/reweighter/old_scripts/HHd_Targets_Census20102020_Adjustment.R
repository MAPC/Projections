###
#Set up
library(tidyverse)
library(tidycensus)
library(srvyr)
library(janitor)
library(data.table)
library(easycensus)
library(mapcdatakeys)
library(readxl)
library(gt)
library(tsibble)
library(feasts)

options(scipen = 999)

root <- "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/"

#MPO Keys
keys <- mapcdatakeys::all_muni_data_keys %>% 
  select(
    muni_id,
    mpo
  )
#RPA Keys
keys_RPA <- mapcdatakeys::all_muni_data_keys %>% 
  select(
    muni_id,
    rpa_acr
  )

#
hh_tba <- read_excel(
  paste0(root,"MPO_Household_Totals_v03.22.23.xlsx"),
  sheet = 1
)

#Household Age and Type Targets
hhd_targets_10 <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2010_V03.22.23.csv")

hhd_targets_20 <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2020_V03.22.23.csv")

hhd_targets_30 <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2030_V03.22.23.csv")

hhd_targets_40 <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2040_V03.22.23.csv")

hhd_targets_50 <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2050_V03.22.23.csv")

###
#MPO ADJUSTMENT
#Create 2010 and 2020 household adjustment factors
hh_2010 <- read_excel(
  paste0(root,"Households_2010_v03.22.23.xlsx"),
  sheet = 1
) %>% 
  select(
    muni_id,
    occhu_10
  ) %>% 
  left_join(
    keys,
    by = c("muni_id")
  ) %>% 
  group_by(
    mpo
  ) %>% 
  summarise(
    hh_occ_2010 = sum(occhu_10)
  ) %>% 
  ungroup() %>% 
  na.omit() 
  
hh_2020 <- read_excel(
  paste0(root,"Households_2020_v03.22.23.xlsx"),
  sheet = 1
) %>% 
  select(
    muni_id,
    hu_occ
  ) %>% 
  left_join(
    keys,
    by = c("muni_id")
  ) %>% 
  group_by(
    mpo
  ) %>% 
  summarise(
    hh_occ_2020 = sum(hu_occ)
  ) %>% 
  ungroup() %>% 
  na.omit()

adj_factors <- left_join(hh_tba, hh_2010, by = c("mpo"))
adj_factors <- left_join(adj_factors, hh_2020, by = c("mpo"))
adj_factors_MPO <- adj_factors %>% 
  rowwise() %>% 
  mutate(
    adj_factor10 = hh_occ_2010/`Sum of proj_hh_2010`,
    adj_factor20 = hh_occ_2020/`Sum of proj_hh_2020`
  ) %>% 
  select(
    c(
      mpo,
      adj_factor10,
      adj_factor20
    )
  ) %>% 
  dplyr::rename(
    RPA = mpo
  )

###

hhd_targets_10_MPO <- left_join(hhd_targets_10, adj_factors_MPO, by = c("RPA")) %>%
  select(
    -adj_factor20
  ) %>% 
  mutate(
    TARGET_adj = TARGET*adj_factor10
  ) %>% 
  select(
    -TARGET,
    -adj_factor10
  ) %>% 
  dplyr::rename(
    TARGET = TARGET_adj
  )

hhd_targets_20_MPO <- left_join(hhd_targets_20, adj_factors_MPO, by = c("RPA")) %>% 
  select(
    -adj_factor10
  )%>% 
  mutate(
    TARGET_adj = TARGET*adj_factor20
  ) %>% 
  select(
    -TARGET,
    -adj_factor20
  ) %>% 
  dplyr::rename(
    TARGET = TARGET_adj
  )
###

write.csv(
  hhd_targets_10_MPO,
  "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2010_MPOadj_V03.22.23.csv",
  row.names = FALSE
)

write.csv(
  hhd_targets_20_MPO,
  "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2020_MPOadj_V03.22.23.csv",
  row.names = FALSE
)

###
#RPA ADJUSTMENT

hh_2010 <- read_excel(
  paste0(root,"Households_2010_v03.22.23.xlsx"),
  sheet = 1
) %>% 
  select(
    muni_id,
    occhu_10
  ) %>% 
  left_join(
    keys_RPA,
    by = c("muni_id")
  ) %>% 
  group_by(
    rpa_acr
  ) %>% 
  summarise(
    hh_occ_2010 = sum(occhu_10)
  ) %>% 
  ungroup() %>% 
  na.omit() 

hh_2020 <- read_excel(
  paste0(root,"Households_2020_v03.22.23.xlsx"),
  sheet = 1
) %>% 
  select(
    muni_id,
    hu_occ
  ) %>% 
  left_join(
    keys_RPA,
    by = c("muni_id")
  ) %>% 
  group_by(
    rpa_acr
  ) %>% 
  summarise(
    hh_occ_2020 = sum(hu_occ)
  ) %>% 
  ungroup() %>% 
  na.omit()

adj_factors <- left_join(hh_tba, hh_2010, by = c("mpo" = "rpa_acr"))
adj_factors <- left_join(adj_factors, hh_2020, by = c("mpo"= "rpa_acr"))
adj_factors_RPA <- adj_factors %>% 
  rowwise() %>% 
  mutate(
    adj_factor10 = hh_occ_2010/`Sum of proj_hh_2010`,
    adj_factor20 = hh_occ_2020/`Sum of proj_hh_2020`
  ) %>% 
  select(
    c(
      mpo,
      adj_factor10,
      adj_factor20
    )
  ) %>% 
  dplyr::rename(
    RPA = mpo
  )

###

hhd_targets_10_RPA <- left_join(hhd_targets_10, adj_factors_RPA, by = c("RPA")) %>%
  select(
    -adj_factor20
  ) %>% 
  mutate(
    TARGET_adj = TARGET*adj_factor10
  ) %>% 
  select(
    -TARGET,
    -adj_factor10
  ) %>% 
  dplyr::rename(
    TARGET = TARGET_adj
  )
  
hhd_targets_20_RPA <- left_join(hhd_targets_20, adj_factors_RPA, by = c("RPA")) %>% 
  select(
    -adj_factor10
  )%>% 
  mutate(
    TARGET_adj = TARGET*adj_factor20
  ) %>% 
  select(
    -TARGET,
    -adj_factor20
  ) %>% 
  dplyr::rename(
    TARGET = TARGET_adj
  )

###

RPA_20_check <- hhd_targets_20_RPA %>% 
  group_by(
    RPA
  ) %>% 
  summarise(
    TARGET = sum(TARGET)
  ) %>% 
  ungroup()

write.csv(
  hhd_targets_10_RPA,
  "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2010_RPAadj_V03.22.23.csv",
  row.names = FALSE
)

write.csv(
  hhd_targets_20_RPA,
  "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_2020_RPAadj_V03.22.23.csv",
  row.names = FALSE
)
