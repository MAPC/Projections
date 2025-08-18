library(tidyverse)
library(tidycensus)
library(magrittr)
library(data.table)
library(janitor)
library(zoo)
library(readxl)

###
#0.1 Set up
options(scipen=999)

census_api_key("d590f657a113f2f78b5a422462ea00745e79111c")

###
#1.1 Load in reweighted PUMS files
#In-office directory
#setwd("//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files")

#Remote directory
setwd("S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Input Files")

#In-office Directory
# setwd("//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files")

rw_2020 <- read.csv(
  "weights_final_Projections2050_2020_V1.csv"
)
rw_2030 <- read.csv(
  "weights_final_Projections2050_2030_V1.csv"
)
rw_2040 <- read.csv(
  "weights_final_Projections2050_2040_V1.csv"
)
rw_2050 <- read.csv(
  "weights_final_Projections2050_2050_V1.csv"
)

#Remote Directory
setwd("S:/Network Shares/DS Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs")

#In-office directory
# setwd("//data-001/public/DataServices/Projects//Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs")

pums_19 <- read.csv("PUMS2019.csv")

pumas10 <- read.csv("PUMA_2010Geog.csv")

pums_19_full <- read.csv("pums_person_file_MA_2019_5yr.csv")

subregion_xwalk <- read_excel("subregions.xlsx",
                              sheet = 1)

#1.2 Clean reweighted PUMS files

rw_2020 %<>%
  dplyr::rename(
   WFACTOR_20 = WFACTOR,
   PWGTP_20 = new_PWGTP,
   WGTP_20 = new_WGTP
  )

rw_2030 %<>%
  dplyr::rename(
    WFACTOR_30 = WFACTOR,
    PWGTP_30 = new_PWGTP,
    WGTP_30 = new_WGTP
  )

rw_2040 %<>%
  dplyr::rename(
    WFACTOR_40 = WFACTOR,
    PWGTP_40 = new_PWGTP,
    WGTP_40 = new_WGTP
  )

rw_2050 %<>%
  dplyr::rename(
    WFACTOR_50 = WFACTOR,
    PWGTP_50 = new_PWGTP,
    WGTP_50 = new_WGTP
  )

rw <- list(
  rw_2020,
  rw_2030,
  rw_2040,
  rw_2050
)

rw <- inner_join(rw_2020, rw_2030, by = c("SERIALNO", "SPORDER"))
rw <- inner_join(rw, rw_2040, by = c("SERIALNO", "SPORDER"))
rw <- inner_join(rw, rw_2050, by = c("SERIALNO", "SPORDER"))

rw %<>%
  mutate(SPORDER = as.character(SPORDER))
###
#2.1 Load 2019 PUMS data

pums_list <- 2019
variable_list <- c("PUMA", "TYPE", "SEX", "AGEP", "RAC1P", "HISP", "ESR", "WKHP",
                   "SCHL", "WAGP", "SEMP", "ADJINC", "SPORDER")


#2019
PUMS_data_5 <- map_dfr(pums_list, ~{
  get_pums(
    variables = variable_list,
    state = "MA",
    survey = "acs5",
    year = .x
  )
},  .id = "year")

PUMS_data_5 %<>%
  filter(TYPE == 1) %>% 
  mutate(SPORDER = as.character(SPORDER))

#PUMS_2019_full_reweighted <- left_join(PUMS_data_5, rw, by = c("SERIALNO", "SPORDER"))

#write.csv(PUMS_2019_full_reweighted,
          #"S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS_2019_full_reweighted.csv")

#2.1.2 2013 PUMS data
pums_list <- 2017
variable_list <- c("PUMA", "TYPE", "SEX", "AGEP", "RAC1P", "HISP", "ESR",
                     "WKHP", "SCHL", "WAGP", "SEMP", "ADJINC", "SPORDER")

PUMS_data_2 <- map_dfr(pums_list, ~{
  get_pums(
    variables = variable_list,
    state = "MA",
    survey = "acs5",
    year = .x
  )
},  .id = "year")

PUMS_data_2 %<>%
  filter(TYPE == 1) %>% 
  mutate(SPORDER = as.character(SPORDER))

###
#3.1 Cleaning PUMS data

#PUMS 2013
PUMS_data_2 <- PUMS_data_2 %>%
  mutate(
    
    #PUMA Adjustment
    #Change variable's data type and pad 0s to the front of the string to match the PUMA crosswalk
    PUMA = as.character(PUMA),
    PUMA = str_pad(PUMA, 5, side = "left", pad = "0"),
    
    # Individual Age Category
    # NOTE: Levels are (1) 0 to 4; (2) 5 to 9; (3) 10 to 14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 29; (7) 30 to 34; (8) 35 to 39; (9) 40 to 44 (10) 45 to 49 (11) 50 to 54 (12) 55 to 59 (13) 60 to 64 (14) 65 to 69 (15) 70 to 74 (16) 75 to 79 (17) 80-84 (18)85p
    AGEP = as.numeric(AGEP), #Change datatype of 
    PAGEC =
      cut(AGEP,
          breaks = c(-Inf, seq(4, 84, 5), Inf),
          labels = 1:18),
    
    # Consolidated Age Category
    # NOTE: Levels are (0) 0-14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 34; (7) 35 to 44; (8) 45 to 54; (9) 55 to 64 (10) 65 to 74 (11) 75+
    PAGEC2 =
      cut(
        AGEP,
        breaks = c(-Inf, 14, 19, 24, 34, 44, 54, 64, 74, 79, 84, Inf),
        labels = c(0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
      ),
    
    #UrbanSim Age Categories
    #NOTE: Levels are (0) 0-14, (1) 15-34, (2) 35-44, (3) 45-64, (4) 65-74, (5) 75+
    PAGEC3 =
      cut(
        AGEP,
        breaks = c(-Inf,14,34,44,64,74,Inf),
        labels = c("0","1","2","3","4","5")
      ),
    
    # Individual Race Category (Analysis may need more specificity than this)
    # NOTE: Categories are (1) White, non hispanic/latino (2)Black, non hispanic/latino (3) hispanic/latino (4)Asian/ Pacific Islander non hispanic/latino (5) other, non hispanic/latino
    PRACE = case_when(
      RAC1P == 1 & HISP == 1 ~ 1,
      RAC1P == 2 & HISP == 1 ~ 2,
      (RAC1P == 6 | RAC1P == 7) & HISP == 1 ~ 4,
      HISP != 1 ~ 3,
      TRUE ~ 5
    ),
    
    # Age, Sex, Race Cat in order of Age Cat, Sex Cat, Race Cat
    #ASRC = paste(PAGEC, SEX, PRACE, sep = "_"),
    # Age, Sex Cat in order of Age Cat, Sex Cat
    #ASC = paste(PAGEC, SEX, sep = "_"),
    # Age, Race Cat in the order of Age Cat, Race Cat
    #ARC = paste(PAGEC, PRACE, sep = "_"),
    
    # Categorizing employment status
    empstat = case_when(
      ESR == 0 ~ "Under 16 not eligible for work",
      ESR == 1 ~ "Employed",
      ESR == 2 ~ "Employed but not at work",
      ESR == 3 ~ "Unemployed",
      ESR == 4 ~ "Armed forces",
      ESR == 5 ~ "Armed forces but not at work",
      ESR == 6 ~ "Not in labor force"
    ),
    
    # Categorizing Civilian Labor Force or not
    # NOTE: Categories are (0) under 16 or non-civilian, (1) in labor force, (2) not in labor force
    lf = case_when(
      ESR == 0 | ESR == 4 | ESR == 5  ~ 0,
      ESR == 1 | ESR == 2 | ESR == 3  ~ 1,
      ESR == 6 ~ 2,
      is.na(ESR) ~ 0
    ),
    
    # Counts as Worker
    # NOTE: Categories are (0) under 16 or not in labor force, or (1) in labor force or non-civilian worker
    worker = case_when(
      (ESR == 0 | ESR == 6) ~ 0,
      (ESR == 1 | ESR == 2 | ESR == 3 | ESR == 4 | ESR == 5) ~ 1,
      is.na(ESR) ~ 0
    ),
    
    # Categorizing full or part-time work
    wrkfull = case_when(
      WKHP < 35 ~ "Part time (less than 35 hrs)",
      WKHP > 34 ~ "Full time (35 hrs or more)",
      is.na(WKHP) ~ "Under 16, unemployed, or not in labor force"
    ),
    
    # Categorizing educational attainment
    # NOTE: Categories are (1) High school degree or less, (2) Associate's degree or some college, (3) Bachelor's, and (4) MA or higher
    SCHL = as.numeric(SCHL),
    eduattn = case_when(
      SCHL <= 17 ~ 1,
      SCHL %in% 18:20 ~ 2,
      SCHL == 21 ~ 3,
      SCHL >= 22 ~ 4,
      is.na(SCHL) ~ 0
    ),
    
    # Adjust wages to 2019 dollars and then to 2013 dollars 
    WAGPA = round(as.numeric(ADJINC) * as.numeric(WAGP), 0),
    
    # Adjust total self employment income to 2019 dollars and then to 2013 dollars
    SWAGPA = round(as.numeric(ADJINC) * as.numeric(SEMP), 0),
    
    # Combine wages and self employment
    WAGPALL = WAGPA + SWAGPA,
    
    # Break wages into wage groups
    WAGPALLC =
      cut(
        WAGPALL,
        breaks = c(-Inf, 0, 10000, 30000, 50000, 75000, 125000, Inf),
        labels = c(seq(0, 6))
      ),
    #UrbanSim Income breakdowns
    #NOTE: Categories are (1) 0-35,000, (2) 35001-75000, (3) 75001-125000, (4) 125001-225000, (5) 225001+
    WAGPALLC_US =
      cut(
        WAGPALL,
        breaks = c(-Inf, 35000, 75000, 125000, 225000, Inf),
        labels = c("1","2","3","4","5")
      ),
    
    # Determine if person is head of household
    HousHder = case_when(SPORDER == 1 ~ 1,
                         SPORDER > 1 ~ 2),
    
    # Determine if case counts as person
    person = case_when(SPORDER != 0 ~ 1,
                       SPORDER == 0 ~ 0),
    
    # Determine if case counts as child
    child = case_when(AGEP < 18 ~ 1,
                      AGEP >= 18 ~ 0)
  ) %>%
  
  #Generating Household Variables
  dplyr::group_by(SERIALNO) %>%
  mutate(
    Worker_Total = sum(worker),
    Person_Total = sum(person),
    Child_Total = sum(child)
  ) %>% 
  
  # Categorizing household sizes by number of persons (1) one person, (2) two persons, (3) three persons, (4) four or more persons
  mutate(  
    HHSize = case_when(
      Person_Total == 1 ~ 1,
      Person_Total == 2 ~ 2,
      Person_Total == 3 ~ 3,
      Person_Total >= 4 ~ 4
    ),
    # Categorizing worker categories (0) no workers, (1) one worker, (2) two workers, (3) three or more workers
    WRKHH = case_when(
      Worker_Total == 0 ~ 0,
      Worker_Total == 1 ~ 1,
      Worker_Total == 2 ~ 2,
      Worker_Total >= 3 ~ 3
    ),
    
    # Determine if person is not hhder vs hhder of 0- wrk, 1wrk, 2 wrk, 3+wrk
    HHder = case_when(
      SPORDER == 1 & WRKHH == 0 ~ 0,
      SPORDER == 1 & WRKHH == 1 ~ 1,
      SPORDER == 1 & WRKHH == 2 ~ 2,
      SPORDER == 1 & WRKHH == 3 ~ 3,
      SPORDER > 1 ~ 99
    ),
    # Determine what type of household the person is heading or not heading (1) head child, (2) child, (3) head not child, (4) not head no child, (5) head single
    HHtype = case_when(
      Child_Total >= 1 & Person_Total > 1 & HHder != 99 ~ 1,
      Child_Total >= 1 & Person_Total > 1 & HHder == 99 ~ 2,
      Child_Total == 0 & Person_Total > 1 & HHder != 99 ~ 3,
      Child_Total == 0 & Person_Total > 1 & HHder == 99 ~ 4,
      Person_Total == 1 & HHder != 99 ~ 5
    ),
    #Change the datatype of SPORDER to character.
    SPORDER = as.character(SPORDER),
    child_flag = case_when(
      Child_Total >= 1 ~ 1,
      Child_Total < 1 ~ 0
    )
  )  %>%
  
  #Selecting for variables used in further analysis
  select(year, TYPE, SERIALNO, SPORDER, WGTP, PWGTP, PUMA, SEX, eduattn, PAGEC, PRACE, lf, WAGPALLC, HHtype, HHder, WRKHH, HHSize, AGEP, PAGEC3, WAGPALLC_US, child_flag)

#Testing for NAs in new variables
sapply(PUMS_data_2, function(x) sum(is.na(x)))

#PUMS 2019
PUMS_data_5 <- PUMS_data_5 %>%
  mutate(
    
    #PUMA Adjustment
    #Change variable's data type and pad 0s to the front of the string to match the PUMA crosswalk
    PUMA = as.character(PUMA),
    #PUMA = str_pad(PUMA, 5, side = "left", pad = "0"),
    
    # Individual Age Category
    # NOTE: Levels are (1) 0 to 4; (2) 5 to 9; (3) 10 to 14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 29; (7) 30 to 34; (8) 35 to 39; (9) 40 to 44 (10) 45 to 49 (11) 50 to 54 (12) 55 to 59 (13) 60 to 64 (14) 65 to 69 (15) 70 to 74 (16) 75 to 79 (17) 80-84 (18)85p
    AGEP = as.numeric(AGEP), #Change datatype of 
    PAGEC =
      cut(AGEP,
          breaks = c(-Inf, seq(4, 84, 5), Inf),
          labels = 1:18),
    
    # Consolidated Age Category
    # NOTE: Levels are (0) 0-14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 34; (7) 35 to 44; (8) 45 to 54; (9) 55 to 64 (10) 65 to 74 (11) 75+
    PAGEC2 =
      cut(
        AGEP,
        breaks = c(-Inf, 14, 19, 24, 34, 44, 54, 64, 74, 79, 84, Inf),
        labels = c(0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
      ),
    
    #UrbanSim Age Categories
    #NOTE: Levels are (0) 0-14, (1) 15-34, (2) 35-44, (3) 45-64, (4) 65-74, (5) 75+
    PAGEC3 =
      cut(
        AGEP,
        breaks = c(-Inf,14,34,44,64,74,Inf),
        labels = c("0","1","2","3","4","5")
      ),
    
    # Individual Race Category (Analysis may need more specificity than this)
    # NOTE: Categories are (1) White, non hispanic/latino (2)Black, non hispanic/latino (3) hispanic/latino (4)Asian/ Pacific Islander non hispanic/latino (5) other, non hispanic/latino
    PRACE = case_when(
      RAC1P == 1 & HISP == 1 ~ 1,
      RAC1P == 2 & HISP == 1 ~ 2,
      (RAC1P == 6 | RAC1P == 7) & HISP == 1 ~ 4,
      HISP != 1 ~ 3,
      TRUE ~ 5
    ),
    
    # Age, Sex, Race Cat in order of Age Cat, Sex Cat, Race Cat
    #ASRC = paste(PAGEC, SEX, PRACE, sep = "_"),
    # Age, Sex Cat in order of Age Cat, Sex Cat
    #ASC = paste(PAGEC, SEX, sep = "_"),
    # Age, Race Cat in the order of Age Cat, Race Cat
    #ARC = paste(PAGEC, PRACE, sep = "_"),
    
    # Categorizing employment status
    empstat = case_when(
      ESR == "b" ~ "Under 16 not eligible for work",
      ESR == 1 ~ "Employed",
      ESR == 2 ~ "Employed but not at work",
      ESR == 3 ~ "Unemployed",
      ESR == 4 ~ "Armed forces",
      ESR == 5 ~ "Armed forces but not at work",
      ESR == 6 ~ "Not in labor force"
    ),
    
    # Categorizing Civilian Labor Force or not
    # NOTE: Categories are (0) under 16 or non-civilian, (1) in labor force, (2) not in labor force
    lf = case_when(
      ESR == "b" | ESR == 4 | ESR == 5  ~ 0,
      ESR == 1 | ESR == 2 | ESR == 3  ~ 1,
      ESR == 6 ~ 2,
      is.na(ESR) ~ 0
    ),
    
    # Counts as Worker
    # NOTE: Categories are (0) under 16 or not in labor force, or (1) in labor force or non-civilian worker
    worker = case_when(
      (ESR == "b" | ESR == 6) ~ 0,
      (ESR == 1 | ESR == 2 | ESR == 3 | ESR == 4 | ESR == 5) ~ 1,
      is.na(ESR) ~ 0
    ),
    
    # Categorizing full or part-time work
    wrkfull = case_when(
      WKHP < 35 ~ "Part time (less than 35 hrs)",
      WKHP > 34 ~ "Full time (35 hrs or more)",
      is.na(WKHP) ~ "Under 16, unemployed, or not in labor force"
    ),
    
    # Categorizing educational attainment
    # NOTE: Categories are (1) High school degree or less, (2) Associate's degree or some college, (3) Bachelor's, and (4) MA or higher
    SCHL = as.numeric(SCHL),
    eduattn = case_when(
      SCHL <= 17 ~ 1,
      SCHL %in% 18:20 ~ 2,
      SCHL == 21 ~ 3,
      SCHL >= 22 ~ 4,
      is.na(SCHL) ~ 0
    ),
    
    # Adjust wages to 2019 dollars and then to 2013 dollars 
    WAGPA = round((as.numeric(ADJINC) * as.numeric(WAGP))*0.911207377, 0),
    
    # Adjust total self employment income to 2019 dollars and then to 2013 dollars
    SWAGPA = round((as.numeric(ADJINC) * as.numeric(SEMP))*0.911207377, 0),
    
    # Combine wages and self employment
    WAGPALL = WAGPA + SWAGPA,
    
    # Break wages into wage groups
    WAGPALLC =
      cut(
        WAGPALL,
        breaks = c(-Inf, 0, 10000, 30000, 50000, 75000, 125000, Inf),
        labels = c(seq(0, 6))
      ),
    #UrbanSim Income breakdowns
    #NOTE: Categories are (1) 0-35,000, (2) 35001-75000, (3) 75001-125000, (4) 125001-225000, (5) 225001+
    WAGPALLC_US =
      cut(
        WAGPALL,
        breaks = c(-Inf, 35000, 75000, 125000, 225000, Inf),
        labels = c("1","2","3","4","5")
      ),
    
    # Determine if person is head of household
    HousHder = case_when(SPORDER == 1 ~ 1,
                         SPORDER > 1 ~ 2),
    
    # Determine if case counts as person
    person = case_when(SPORDER != 0 ~ 1,
                       SPORDER == 0 ~ 0),
    
    # Determine if case counts as child
    child = case_when(AGEP < 18 ~ 1,
                      AGEP >= 18 ~ 0)
  ) %>%
  
  #Generating Household Variables
  dplyr::group_by(SERIALNO) %>%
  mutate(
    Worker_Total = sum(worker),
    Person_Total = sum(person),
    Child_Total = sum(child)
  ) %>% 
  
  # Categorizing household sizes by number of persons (1) one person, (2) two persons, (3) three persons, (4) four or more persons
  mutate(  
    HHSize = case_when(
      Person_Total == 1 ~ 1,
      Person_Total == 2 ~ 2,
      Person_Total == 3 ~ 3,
      Person_Total >= 4 ~ 4
    ),
    # Categorizing worker categories (0) no workers, (1) one worker, (2) two workers, (3) three or more workers
    WRKHH = case_when(
      Worker_Total == 0 ~ 0,
      Worker_Total == 1 ~ 1,
      Worker_Total == 2 ~ 2,
      Worker_Total >= 3 ~ 3
    ),
    
    # Determine if person is not hhder vs hhder of 0- wrk, 1wrk, 2 wrk, 3+wrk
    HHder = case_when(
      SPORDER == 1 & WRKHH == 0 ~ 0,
      SPORDER == 1 & WRKHH == 1 ~ 1,
      SPORDER == 1 & WRKHH == 2 ~ 2,
      SPORDER == 1 & WRKHH == 3 ~ 3,
      SPORDER > 1 ~ 99
    ),
    # Determine what type of household the person is heading or not heading (1) head child, (2) child, (3) head not child, (4) not head no child, (5) head single
    HHtype = case_when(
      Child_Total >= 1 & Person_Total > 1 & HHder != 99 ~ 1,
      Child_Total >= 1 & Person_Total > 1 & HHder == 99 ~ 2,
      Child_Total == 0 & Person_Total > 1 & HHder != 99 ~ 3,
      Child_Total == 0 & Person_Total > 1 & HHder == 99 ~ 4,
      Person_Total == 1 & HHder != 99 ~ 5
    ),
    #Change the datatype of SPORDER to character.
    SPORDER = as.character(SPORDER),
    child_flag = case_when(
      Child_Total >= 1 ~ 1,
      Child_Total < 1 ~ 0
    )
  )  %>%
  
  #Selecting for variables used in further analysis
  select(year, TYPE, SERIALNO, SPORDER, WGTP, PWGTP, PUMA, SEX, eduattn, PAGEC, PRACE, lf, WAGPALLC, HHtype, HHder, WRKHH, HHSize, AGEP, PAGEC3, WAGPALLC_US, child_flag)

#Testing for NAs in new variables
sapply(PUMS_data_5, function(x) sum(is.na(x)))

#3.2 Merges the cleaned PUMS data to the projection year weights
PUMS_data_5m <- left_join(PUMS_data_5, rw, by = c("SERIALNO", "SPORDER"))

#3.3 Cleans the PUMA file
#Creates a list of all RPAs
RPA <- c("MAPC", "OCPC", "SRPEDD", "MVC", "NPEDC", "NMCOG", "MVPC", "MRPC", 
         "CMRPC", "FRCOG", "PVPC", "BRPC", "CCC")

pumas10 <- pumas10 %>%
  select(name_t, all_of(RPA)) %>% #Selects names of relevant variables
  rename(PUMA = name_t) %>% #Renames variable representing distinct PUMAs
  mutate(PUMA = as.character(PUMA), #Changes the datatype to character
         PUMA = str_pad(PUMA, 5, side = "left", pad = "0")) %>% #Adds leading 0s to match strings with xwalk data
  pivot_longer(cols = all_of(RPA), names_to = "RPA", values_to = "in_RPA") %>%
  filter(in_RPA == 1) %>%
  select(-in_RPA) #Assigns a RPA to each PUMA

#3.4 Merges the geography file with the PUMS data
PUMS_data_2m <- left_join(PUMS_data_2, pumas10, by = c("PUMA"))

PUMS_data_2m %<>%
  na.omit()

PUMS_data_5mm <- left_join(PUMS_data_5m, pumas10, by = c("PUMA"))

PUMS_data_5mm %<>%
  na.omit()

###
#4.1 Summarizing Projected Data

Control_2010 <- PUMS_data_5mm %>%
  group_by(
    RPA,
    PAGEC3,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = PWGTP
  ) %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      PAGEC3 == 1 ~ 15,
      PAGEC3 == 2 ~ 35,
      PAGEC3 == 3 ~ 45,
      PAGEC3 == 4 ~ 65,
      PAGEC3 == 5 ~ 75
    ),
    age_of_head_max = case_when(
      PAGEC3 == 1 ~ 34,
      PAGEC3 == 2 ~ 44,
      PAGEC3 == 3 ~ 64,
      PAGEC3 == 4 ~ 74,
      PAGEC3 == 5 ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35001,
      WAGPALLC_US == 3 ~ 75001,
      WAGPALLC_US == 4 ~ 125001,
      WAGPALLC_US == 5 ~ 225001
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2010
  )
  
Control_2020 <- PUMS_data_5mm %>% 
  group_by(
    RPA,
    PAGEC3,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = PWGTP_20
  ) %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      PAGEC3 == 1 ~ 15,
      PAGEC3 == 2 ~ 35,
      PAGEC3 == 3 ~ 45,
      PAGEC3 == 4 ~ 65,
      PAGEC3 == 5 ~ 75
    ),
    age_of_head_max = case_when(
      PAGEC3 == 1 ~ 34,
      PAGEC3 == 2 ~ 44,
      PAGEC3 == 3 ~ 64,
      PAGEC3 == 4 ~ 74,
      PAGEC3 == 5 ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35001,
      WAGPALLC_US == 3 ~ 75001,
      WAGPALLC_US == 4 ~ 125001,
      WAGPALLC_US == 5 ~ 225001
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2020
  )

Control_2030 <- PUMS_data_5mm %>% 
  group_by(
    RPA,
    PAGEC3,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = PWGTP_30
  ) %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      PAGEC3 == 1 ~ 15,
      PAGEC3 == 2 ~ 35,
      PAGEC3 == 3 ~ 45,
      PAGEC3 == 4 ~ 65,
      PAGEC3 == 5 ~ 75
    ),
    age_of_head_max = case_when(
      PAGEC3 == 1 ~ 34,
      PAGEC3 == 2 ~ 44,
      PAGEC3 == 3 ~ 64,
      PAGEC3 == 4 ~ 74,
      PAGEC3 == 5 ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35001,
      WAGPALLC_US == 3 ~ 75001,
      WAGPALLC_US == 4 ~ 125001,
      WAGPALLC_US == 5 ~ 225001
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2030
  )

Control_2040 <- PUMS_data_5mm %>% 
  group_by(
    RPA,
    PAGEC3,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = PWGTP_40
  ) %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      PAGEC3 == 1 ~ 15,
      PAGEC3 == 2 ~ 35,
      PAGEC3 == 3 ~ 45,
      PAGEC3 == 4 ~ 65,
      PAGEC3 == 5 ~ 75
    ),
    age_of_head_max = case_when(
      PAGEC3 == 1 ~ 34,
      PAGEC3 == 2 ~ 44,
      PAGEC3 == 3 ~ 64,
      PAGEC3 == 4 ~ 74,
      PAGEC3 == 5 ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35001,
      WAGPALLC_US == 3 ~ 75001,
      WAGPALLC_US == 4 ~ 125001,
      WAGPALLC_US == 5 ~ 225001
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2040
  )

Control_2050 <- PUMS_data_5mm %>% 
  group_by(
    RPA,
    PAGEC3,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = PWGTP_50
  ) %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      PAGEC3 == 1 ~ 15,
      PAGEC3 == 2 ~ 35,
      PAGEC3 == 3 ~ 45,
      PAGEC3 == 4 ~ 65,
      PAGEC3 == 5 ~ 75
    ),
    age_of_head_max = case_when(
      PAGEC3 == 1 ~ 34,
      PAGEC3 == 2 ~ 44,
      PAGEC3 == 3 ~ 64,
      PAGEC3 == 4 ~ 74,
      PAGEC3 == 5 ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35001,
      WAGPALLC_US == 3 ~ 75001,
      WAGPALLC_US == 4 ~ 125001,
      WAGPALLC_US == 5 ~ 225001
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2050
  )

Control_all <- rbind(Control_2010, Control_2020, Control_2030, Control_2040, Control_2050)

Control_all_expand <- Control_all %>%
  select(PAGEC3, WAGPALLC_US, HHsize, child_flag, year, RPA) %>% 
  group_by(PAGEC3, WAGPALLC_US, HHsize, child_flag, RPA) %>% 
  mutate(year = as.double(year)) %>% 
  tidyr::complete(year = tidyr::full_seq(2010:2050, 1)) %>% 
  filter(year != "1") %>% 
  filter(year != "2") %>% 
  filter(year != "3") %>% 
  filter(year != "4") #%>% 
  #mutate(
    #year = as.factor(year)
  #)

Control_all_expand_m <- left_join(Control_all_expand, Control_all, by = c("PAGEC3", "WAGPALLC_US", "HHsize", "child_flag", "year", "RPA"))

Control_all_expand_m %<>% 
  mutate(
    age_of_head_min = case_when(
      PAGEC3 == 1 ~ 15,
      PAGEC3 == 2 ~ 35,
      PAGEC3 == 3 ~ 45,
      PAGEC3 == 4 ~ 65,
      PAGEC3 == 5 ~ 75
    ),
    age_of_head_max = case_when(
      PAGEC3 == 1 ~ 34,
      PAGEC3 == 2 ~ 44,
      PAGEC3 == 3 ~ 64,
      PAGEC3 == 4 ~ 74,
      PAGEC3 == 5 ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35001,
      WAGPALLC_US == 3 ~ 75001,
      WAGPALLC_US == 4 ~ 125001,
      WAGPALLC_US == 5 ~ 225001
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    )
  )
  
##interpolate to fill in NAs for non-decadial years
Full_Control_HHds<- Control_all_expand_m %>% 
  group_by(PAGEC3, WAGPALLC_US, HHsize, child_flag, RPA) %>%
  mutate(n = zoo::na.approx(n, na.rm = FALSE)) %>% 
  filter(PAGEC3 != "0")

Full_Control_HHds <- left_join(Full_Control_HHds, subregion_xwalk, by = c("RPA"))
Full_Control_HHds %<>%
  ungroup(
  ) %>% 
  select(
    -RPA,
    -PAGEC3,
    -WAGPALLC_US,
    -HHsize,
    -child_flag,
    -subregion_code_mapc
  ) %>% 
  dplyr::rename(
    total_number_of_households = n,
    subregion_code = subregion_code_urbansim
  ) %>% 
  relocate(
    year,
    subregion_code,
    total_number_of_households,
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max
  )

#Testing for NAs in new variables
sapply(Full_Control_HHds, function(x) sum(is.na(x)))

#Diagnosing NAs
TEST_NA_Full_Control_HHds <- Full_Control_HHds[is.na(Full_Control_HHds$income_max),]
#Remote
write.csv(Full_Control_HHds,
"S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Output Files/Control_HHds_Projections2050_v5.csv",row.names=F)
#In-office
# write.csv(Full_Control_HHds,
#           "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Output Files/Control_HHds_Projections2050_v4.csv",
#           row.names = FALSE)
