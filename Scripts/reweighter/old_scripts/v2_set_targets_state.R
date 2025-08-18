library(data.table)
library(readxl)
library(tidyverse)
library(magrittr)

root <- 'S:/Network Shares/DS Projects/Current_Projects/'
# root <- '//data-001/public/DataServices/Projects/Current_Projects/'
setwd(paste0(root, 'Projections/Reweighter/Input_Files/v2/'))


# mpos <-
#   mapcdatakeys::all_muni_data_keys %>% select(mpo) %>% unique()
m <- 'MVPC'
yr <- 2020

# for (m in mpos$mpo) {
#   pums <- fread(paste0('PUMS2019_',m,'.csv'))
#   pums[,totPOP:=1]
#   pums[is.na(lf),lf:=0]
#   fwrite(pums, paste0(m,'/PUMS2019_',m,'.csv'))
  
  # file.copy(paste0('reweighting_config_2019_',m,'.json'),paste0(m,'/reweighting_config_2019_',m,'.json'))
  
# }


    # Population Targets
    pums <- fread(paste0(m,'/PUMS2019_',m,'.csv')) 
    pa <- pums %>% 
      group_by(ageCAT3) %>% 
      summarise(BASELINE = sum(PWGTP)) %>% 
      mutate(ageCAT3 = paste0('x == ',ageCAT3))
    
    tpa <- read.csv(paste0('Pop_Age_',yr,'_V2.csv'))
    
    tpa <- tpa %>%
      filter(RPA == m) %>%
      dplyr::group_by(ageCAT3) %>%
      summarise(TARGET = sum(TARGET))
    
    pa <- left_join(pa, tpa, by = c("ageCAT3")) %>% 
      mutate(INTER = BASELINE)
    write.csv(pa, paste0(m,'/Pop_Age.csv'))
    
    tpop <- read.csv(paste0(m,'/Pop_Age.csv')) %>% 
      select(BASELINE, TARGET, INTER) %>% 
      summarise(BASELINE = sum(BASELINE),
                TARGET = sum(TARGET),
                INTER = sum(INTER)) %>% 
      mutate(totPOP = 'x == 1') %>% 
      relocate(totPOP, .before = BASELINE)
    write.csv(tpop, paste0(m,'/Pop_Total.csv'))
      

    # # Labor Force Targets
    # as <- pums %>% 
    #   filter(lf == 1) %>% 
    #   group_by(ageCAT3, edu, lf) %>% 
    #   summarise(BASELINE = sum(PWGTP)) %>% 
    #   mutate(ageCAT3 = paste0('x == ',ageCAT3),
    #          edu = paste0('x == ',edu),
    #          lf = paste0('x == ',lf))
    # 
    # tas <- read.csv(paste0('LF_Age_Edu_',yr,'_V2.csv'))
    # 
    # tas <- tas %>%
    #   filter(RPA == m) %>%
    #   select(ageCAT3, edu, lf, TARGET) %>% 
    #   group_by(ageCAT3,edu,lf) %>%
    #   summarise(TARGET = sum(TARGET))
    # as <- left_join(as, tas, by = c("ageCAT3", "edu", "lf")) %>%
    #   mutate(INTER = BASELINE) %>%
    #   replace(is.na(.),
    #           0)
    # 
    # write.csv(as, paste0(m,'/Age_Edu.csv'))
    # 
    # # Household Type Targets
    # hh <- pums %>% 
    #   filter(SPORDER==1) %>% 
    #   group_by(ageCAT3, HHtype) %>% 
    #   summarise(BASELINE = sum(WGTP)) %>% 
    #   mutate(ageCAT3 = paste0('x == ',ageCAT3),
    #          HHtype = paste0('x == ',HHtype))
    # 
    # thh <- read.csv(paste0('AgeHHder_HHtype_',yr,'_V3.csv'))
    # 
    # thh <- thh %>%
    #   select(-BASELINE,-INTER) %>% 
    #   filter(RPA == m) %>%
    #   group_by(ageCAT3, HHtype) %>%
    #   summarise(TARGET = sum(TARGET))
    # 
    # hh <- left_join(hh, thh, by = c("ageCAT3", "HHtype"))%>%
    #   mutate(INTER = BASELINE)
    # 
    # write.csv(hh, paste0(m,'/AgeHHder_HHtype.csv'))
    # 
    # Labor Force Targets
    as <- read.csv(paste0(m,'/Age_Edu.csv'))
    tas <- read.csv(paste0('LF_Age_Edu_',yr,'_V2.csv'))

    tas <- tas %>%
      filter(RPA == m) %>%
      dplyr::mutate(
        ageCAT3 = as.factor(ageCAT3),
        edu = as.factor(edu),
        lf = as.factor(lf)
      ) %>%
      dplyr::group_by(ageCAT3,
                      edu,
                      lf) %>%
      summarise(TARGET = sum(TARGET),
                BASELINE = sum(BASELINE))

    as <- left_join(as, tas, by = c("ageCAT3", "edu", "lf"))

    as %<>%
      select(ageCAT3,edu,lf,TARGET.y,BASELINE.y) %>%
      dplyr::rename(TARGET = TARGET.y,
                    BASELINE = BASELINE.y) %>%
      mutate(INTER = BASELINE) %>%
      replace(is.na(.),
              0)

    write.csv(as, paste0(m,'/Age_Edu.csv'))

    # HHder Age + Household Type Targets
    hh <- read.csv(paste0(m,'/AgeHHder_HHtype.csv'))
    thh <- read.csv(paste0('AgeHHder_HHtype_',yr,'_V3.csv'))

    thh <- thh %>%
      filter(RPA == m) %>%
      dplyr::mutate(ageCAT3 = as.factor(ageCAT3),
                    HHtype = as.factor(HHtype)) %>%
      dplyr::group_by(ageCAT3,
                      HHtype) %>%
      summarise(TARGET = sum(TARGET),
                BASELINE = sum(BASELINE))

    hh <- left_join(hh, thh, by = c("ageCAT3", "HHtype"))

    hh %<>%
      select(ageCAT3,HHtype,TARGET.y,BASELINE.y) %>%
      dplyr::rename(TARGET = TARGET.y,
                    BASELINE = BASELINE.y,) %>%
      mutate(INTER = BASELINE)

    write.csv(hh, paste0(m,'/AgeHHder_HHtype.csv'))

    # Household Type Targets
    hh <- read.csv(paste0(m,'/AgeHHder_HHtype.csv')) %>% 
      group_by(HHtype) %>% 
      summarise(TARGET = sum(TARGET),
                BASELINE = sum(BASELINE),
                INTER = sum(INTER))
    
    thh <- read.csv(paste0('AgeHHder_HHtype_',yr,'_V3.csv'))
    
    thh <- thh %>%
      filter(RPA == m) %>%
      dplyr::mutate(HHtype = as.factor(HHtype)) %>%
      dplyr::group_by(HHtype) %>%
      summarise(TARGET = sum(TARGET),
                BASELINE = sum(BASELINE))
    
    hh <- left_join(hh, thh, by = c("HHtype"))
    
    hh %<>%
      select(HHtype,TARGET.y,BASELINE.y) %>%
      dplyr::rename(TARGET = TARGET.y,
                    BASELINE = BASELINE.y,) %>%
      mutate(INTER = BASELINE)
    
    write.csv(hh, paste0(m,'/HHtype.csv'))