setwd('S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter')
library(data.table)
library(tidyverse)
library(tidycensus)

# mpo list
ms <-
  mapcdatakeys::all_muni_data_keys %>% select(mpo) %>% unique()
mpos <- sort(ms$mpo)

#
sf1 <- fread('')
# Load muni control totals and sum households by age of HHder
ux <- fread('subregions_crosswalk.csv') %>% rename(subregion_code = subregion_code_urbansim, mpo = RPA)
ct <- fread('MAPC101_municipal_control_totals_2010_v5.csv') %>% left_join(ux, by='subregion_code') %>% setDT()
ct <- ct[,lapply(.SD,sum),.(mpo,muni_id,age_of_head_max),.SDcols='total_number_of_households']
setkey(ct,mpo)

# Summarize total households by mpo from target inputs to reweighter
thh <- fread('AgeHHder_HHtype_2020_adj_V03.22.23.csv')
tsum <- thh[,lapply(.SD,sum),RPA,.SDcols='TARGET']
tsum[,TARGET:=round(TARGET)]
setnames(tsum,'RPA','mpo')
setkey(tsum,mpo)

# Calculate total households from the reweighter outputs
pums <- fread('PUMS2019_formatted_20230322.csv')
setkey(pums,SERIALNO,SPORDER)
rw <- data.table()
for (m in mpos){
  x <- fread(paste0(m,'/weights_final_Projections2050_muni_',m,'_2020.csv')) %>% select(SERIALNO, SPORDER, new_WGTP) %>% mutate(mpo=m) %>% setDT()
  setkey(x,SERIALNO,SPORDER)
  xp <- pums[x]
  xsum <- xp[SPORDER==1, lapply(.SD,sum), .(mpo), .SDcols='new_WGTP']
  xsum[,rw_HH:=round(new_WGTP)][,new_WGTP:=NULL]
  rw <- rbind(rw,xsum)
}
setkey(rw,mpo)



# PL94 total households
munis <- mapcdatakeys::all_muni_data_keys %>% select(muni_id, muni_name, cosub_cn10, cosub_cn20, mapc, mpo, rpa_acr) %>% mutate(GEOID = as.character(cosub_cn20))
pl94 <-
  get_decennial(
    year = 2020,
    state = 'MA',
    geography = 'county subdivision',
    variables = 'H1_002N'
  ) %>%
  mutate(
    variable = case_when(
      variable == 'H1_002N' ~ "pl94_HH_2020"
    )
  ) %>%
  pivot_wider(names_from = 'variable') %>% left_join(munis, by='GEOID') %>% select(rpa_acr,mpo,pl94_HH_2020) %>% na.omit() %>% setDT()
pl94 <- pl94[,lapply(.SD,sum,na.rm=T),mpo,.SDcols='pl94_HH_2020']
setkey(pl94,mpo)


# Join all datasets
comp <- ct[rw][tsum][pl94]
setnames(comp,c('mpo','control_total_HH','ReweighterOutput_HH','TARGET_HH','pl94_HH'))
fwrite(comp,'targets_vs_control_totals_2020.csv')

