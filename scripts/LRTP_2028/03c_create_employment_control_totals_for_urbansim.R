##Script to interpolate new employment control totals from updated 2018 base
##August 28, 2022
## Brandon Stanaway

# turn off scientific notation
options(scipen = 999)

# load packages
pacman::p_load(readxl, RPostgreSQL, tidyverse, zoo, data.table, magrittr)

#Remote
#setwd('S:/Network Shares/DS Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs')

#In-office
setwd("K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs")

emp <- read_excel("employment_projections_Projections2050_v03.24.23.xlsx",
                  sheet = 1)

subregion_xwalk <- read_excel("subregions.xlsx",
                              sheet = 1)
###
#1.0 Construct the 2010 Baseline Control data for the MPO (MAPC97) and RPA (MAPC101)
#Data Axle Adjusted ES_202 Data

DAadj_root <- "K:/DataServices/Datasets/Economy/InfoGroup Business Data/Analysis/dataaxle_2019_revisions/"
#Amasses the 2010-2019 MPO Baseline Data
emp_baseline_MPO <- data.frame()

for (yr in c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) {
  tmp <- read.csv(paste0(DAadj_root,"es202_adjusted_employment_by_MPO_superNAICS_",yr,".csv"))
  
  tmp <- tmp %>% 
    #Renames columns to what we need to have in the controls
    rename_with(
      .cols = c(1,2,3),
      ~c("aggr_sector_id","RPA","total_number_of_jobs")
    ) %>%
    #Adds on a "year" column
    mutate(
      year = yr
    )
  
  emp_baseline_MPO <- bind_rows(emp_baseline_MPO,tmp)
}

emp_baseline_MPO %<>%
  left_join(subregion_xwalk, by = c("RPA")) %>%
  select(
    -RPA,
    -subregion_code_mapc
  ) %>% 
  dplyr::rename(
    subregion_code = subregion_code_urbansim
  )

#Amasses the 2010-2019 RPA Baseline Data
emp_baseline_RPA <- data.frame()

for (yr in c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) {
  tmp <- read.csv(paste0(DAadj_root,"es202_adjusted_employment_by_RPA_superNAICS_",yr,".csv"))
  
  tmp <- tmp %>% 
    #Renames columns to what we need to have in the controls
    rename_with(
      .cols = c(1,2,3),
      ~c("aggr_sector_id","RPA","total_number_of_jobs")
    ) %>%
    #Adds on a "year" column
    mutate(
      year = yr
    )
  emp_baseline_RPA <- rbind(emp_baseline_RPA,tmp)
}

emp_baseline_RPA %<>%
  left_join(subregion_xwalk, by = c("RPA")) %>%
  select(
    -RPA,
    -subregion_code_mapc
  ) %>% 
  dplyr::rename(
    subregion_code = subregion_code_urbansim
  ) %>% 
  filter(
    subregion_code == 7
  )

###
#2.0 Data cleaning of UMDI Employment Projections

emp <- left_join(emp, subregion_xwalk, by = c("rpa" = "RPA"))

emp %<>%
  select(
    -rpa,
    -subregion_code_mapc
  ) %>% 
  dplyr::rename(
    aggr_sector_id = naics,
    subregion_code = subregion_code_urbansim
  ) %>% 
  relocate(
    subregion_code,
    .before = total_number_of_jobs
  )

#3.0 Expand Projected Data and Interpolate interdecadal jobs data
#Creates expanded and interpolated MPO Data
emp_MPO <- emp %>% 
  group_by(
    aggr_sector_id,
    subregion_code
  ) %>%
  #Expands the number of years between 2020 and 2050 to include all inter-
  #decadal years
  tidyr::complete(
    year = tidyr::full_seq(2020:2050,1)
  ) %>% 
  mutate(
    #Interpolates data for the number of jobs in those years by the variables
    #in the group_by() call. 
    total_number_of_jobs = zoo::na.approx(total_number_of_jobs, na.rm = FALSE)
  )

#Binds the 2010-2019 data to the 2020-2050 data
emp_MPO_full <- bind_rows(emp_baseline_MPO, emp_MPO)

emp_MPO_full %<>%
  mutate(
    #Round the calues in the number of jobs variable
    total_number_of_jobs = round(total_number_of_jobs,0)
  ) %>% 
  relocate(
    #Orders the columns
    year,
    aggr_sector_id,
    subregion_code,
    total_number_of_jobs
  )

#Export File
write.csv(emp_MPO_full,
          "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Output_Files/Statewide_Employment_Controls_v03.29.23.csv",
          row.names = FALSE)

#Creates expanded and interpolated RPA Data
#Create adjustment factor for MAPC
#Loads the total employment by super sector for each of the rebel towns
rebel_towns_emp <- read.csv("K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/rebel_towns_2020_es202_emp_by_superNAICS.csv")

#Aggregates employment of the rebel towns into each super sector
rebel_towns_emp %<>%
  group_by(
    RPA,
    superNAICS
  ) %>% 
  summarise(
    total_number_of_jobs = sum(es202_2020r)
  ) %>% 
  ungroup() %>% 
  dplyr::rename(
    subregion_code = RPA,
    aggr_sector_id = superNAICS
  ) %>% 
  mutate(
    subregion_code = 7
  )

#Creates a data frame of 2020 super sector employment for MAPC
emp_MAPC <- emp %>% 
  filter(
    subregion_code == 7 & year == 2020
  )

#Join the employment from the rebel towns to the MAPC employment data by super sector
MAPC101_adj_factors <- left_join(emp_MAPC, rebel_towns_emp, by = c("aggr_sector_id","subregion_code")) %>% 
  dplyr::rename(
    #Rename a variable to reflect the number of jobs in MAPC97
    total_number_of_jobs_MAPC97 = total_number_of_jobs.x,
    #Rename a variable to reflect the number of jobs in the rebel towns
    total_number_of_jobs_RT = total_number_of_jobs.y
  ) %>% 
  mutate(
    #Fill NAs with 0 (Super sector 4 (Government is 0 for the rebel towns))
    total_number_of_jobs_RT = if_else(is.na(total_number_of_jobs_RT), 0, total_number_of_jobs_RT),
    #Create a variable to reflect the total employment by super sector for what
    #would be MAPC101 (MAPC97 + the four rebel towns)
    total_number_of_jobs_MAPC101 = total_number_of_jobs_MAPC97 + total_number_of_jobs_RT,
    #Create a variable that is the percent of jobs the rebel towns make up
    #as a proportion of MAPC101 employment by super sector. This is the
    #adjustment factor that will be applied to projected decadal employment
    #numbers.
    adj_factor = total_number_of_jobs_MAPC101/total_number_of_jobs_MAPC97
  ) %>% 
  select(
    aggr_sector_id,
    subregion_code,
    adj_factor
  )

emp_RPA <- emp %>% 
  #Filters out MAPC from the full employment projections data frame. 
  filter(
    subregion_code == 7
  ) %>% 
  #Joins the MAPC101 adjustment factors calculated with the code block on lines
  #176-200.
  left_join(
    MAPC101_adj_factors,
    by = c("aggr_sector_id","subregion_code")
  ) %>%
  rowwise() %>% 
  #Multiply jobs in each super sector for each projections decade by the
  #adjustment factors.
  mutate(
   total_number_of_jobs_adj = round(total_number_of_jobs*adj_factor,0) 
  ) %>% 
  #Remove excess variables now that the adjustment calculation is complete.
  select(
    -c(
      adj_factor,
      total_number_of_jobs
    )
  ) %>% 
  #Rename the variable containing information about the adjust jobs in each
  #super sector and year.
  dplyr::rename(
    total_number_of_jobs = total_number_of_jobs_adj
  ) %>% 
  group_by(
    aggr_sector_id,
    subregion_code
  ) %>% 
  #Expand the data frame to include inter-decadal years.
  tidyr::complete(
    year = tidyr::full_seq(2020:2050,1)
  ) %>% 
  #Interpolate jobs data into the inter-decadal years by super sector.
  mutate(
    total_number_of_jobs = zoo::na.approx(total_number_of_jobs, na.rm = FALSE),
    total_number_of_jobs = round(total_number_of_jobs,0)
  ) %>% 
  ungroup()

#Binds the 2010-2019 RPA Employment data to the 2020-2050 Projected Employment data.  
emp_RPA_full <- bind_rows(emp_baseline_RPA, emp_RPA)
  
emp_RPA_full %<>%
  #Rounds the values in the variable containing information about the total
  #number of jobs.
  mutate(
    total_number_of_jobs = round(total_number_of_jobs,0)
  ) %>% 
  select(
    -c(
      subregion_code
    )
  ) %>% 
  relocate(
    year,
    aggr_sector_id,
    total_number_of_jobs
  )

#Export File
write.csv(emp_RPA_full,
          "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Output_Files/MAPC101_Employment_Controls_v03.30.23.csv",
          row.names = FALSE)
################################################################################