library(zoo)
library(data.table)
library(tidyverse)
library(magrittr)

dir <- 'S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/'
# dir <- 'K:/DataServices/Projects/Current_Projects/Projections/Reweighter/'

setwd(dir)
#Change data thats loaded in to reflect the most recent statewide Household Control dataset.
hhd_control <- fread(paste0(dir,'Output_Files/Statewide_Control_HHds_Projections2050_MB_v03.23.23.csv'))

scaling_totals <- fread(paste0(dir,'Input_Files/massbuilds_unit_growth_factors.csv'))
setkey(scaling_totals,year,mapc)

hhd_control %<>% 
  mutate(
    mapc = case_when(
      subregion_code == 7 ~ 1,
      subregion_code != 7 ~ 0
    )
  ) 

hh20 <- hhd_control %>% filter(year <= 2020)
setDT(hh20)

hh20 <- hh20[year!=2019]
hh20[year==2020,year:=2019]
hh20[,grp:=.GRP,.(age_of_head_max,income_max,persons_min,persons_max,children_min,children_max)]
hh20[year==2010,h10:=sum(total_number_of_households),.(grp,subregion_code)]
hh20[,h10:=na.locf(h10),.(grp,subregion_code)]
hh20[year==2019,h19:=sum(total_number_of_households),.(grp,subregion_code)]
hh20[,h19:=na.locf(h19),.(grp,subregion_code)]
hh20[,diff:=h19-h10]
setkey(hh20,year,mapc)
hh20 <- scaling_totals[hh20]
hh20[,tot2:=shift(total_number_of_households),.(grp,subregion_code)]

hh20[year %in% 2011:2018,total_number_of_households:=round((diff*scaling_factor)+tot2,0),.(grp,subregion_code)]

hh20[,`:=`(grp=NULL,h10=NULL,h19=NULL,diff=NULL,tot2=NULL,scaling_factor=NULL)]
hh20p <- hhd_control %>% filter(year > 2020) %>% mutate(year = year-1)
hh50 <- hhd_control %>% filter(year == 2050)
hhd_control <- rbind(hh20,hh20p,hh50)
hhd_control[,mapc:=NULL]

hhd_control_update <- hhd_control %>% 
  mutate(
    total_number_of_households = round(total_number_of_households, 0)
  )

write.csv(hhd_control_update,
          paste0(dir,"Output_Files/Statewide_Control_HHds_Projections2050_MB_v03.23.23_one_year_offset.csv"),
          row.names = FALSE)

