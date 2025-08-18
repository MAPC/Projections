library(mapcdatakeys)
pacman::p_load(tidyverse,
               tidycensus,
               sf,
               leaflet,
               tigris,
               htmlwidgets,
               viridis,
               data.table)
setwd('K:/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim')
# setwd('S:/Network Shares/DS Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim')

# Update of Massbuilds inputs to UrbanSim to ensure consistency with current MB database as of date in 'vintage'

vintage <- '20230929'
dir.create(paste0('MassBuilds/Massbuilds_inputs_',vintage),showWarnings = F)
dir.create(paste0('MassBuilds/Massbuilds_inputs_',vintage,'/Statewide/'),showWarnings = F)
dir.create(paste0('MassBuilds/Massbuilds_inputs_',vintage,'/MAPC/'),showWarnings = F)

# new massbuilds data
res <- fread(paste0('MassBuilds/Massbuilds_inputs_',vintage,'/Statewide/',vintage,'_residential.csv'))
com <- fread(paste0('MassBuilds/Massbuilds_inputs_',vintage,'/Statewide/',vintage,'_non_residential.csv'))

# previous massbuilds data
# ores <- fread(paste0('MassBuilds/Massbuilds_inputs_',vintage,'/mb_20221121_residential.csv'))
# ocom <- fread(paste0('MassBuilds/Massbuilds_inputs_',vintage,'/mb_20221121_non_residential.csv'))

#
res[is.na(stalled),stalled:=FALSE]
res <- res[stalled!=T]
com[is.na(stalled),stalled:=FALSE]
com <- com[stalled!=T]

# QA reviewed Massbuilds data from Rose's massbuilds review
# 1st file contains developments where MB==2010 and parcel data 'yr_compl' is zero or >=2010
adj <- fread('Massbuilds/massbuilds_parcel2010data.csv')
adj <- adj[,.(id,year_compl)]
setnames(adj,c('identification','newstart'))
adj[newstart==2010,newstart:=2011]
setkey(adj,identification)
# 2nd file is MB==2010 and parcel data yr_compl<2010
pre10 <- fread('Massbuilds/massbuilds_before2010_reviewed.csv')
pre10[yr_built==2010,yr_built:=2009]
pre10[,newstart:=yr_built]
pre10 <- pre10[,.(identification,newstart)]
setkey(pre10,identification)
ad <- rbind(adj,pre10)
ad <- unique(ad)
setkey(ad,identification)

### Adjust massbuilds files for bulk upload to UrbanSim

##
# Residential
##
r10 <- res[start_year==2010]
res <- res[start_year!=2010]
# Drop pre-2010 projects and projects with built_year==0
setkey(r10,identification)
r10 <- ad[r10]
r10[!is.na(newstart),start_year:=newstart]
r10[is.na(newstart),start_year:=2011]
r10 <- r10[start_year>2010]
r10[,newstart:=NULL]
res <- rbind(r10,res)
res[city == 'Manchester', city := 'Manchester-by-the-Sea']

##
# Non-residential projects
##
cmb <- fread('MassBuilds/massbuilds_non_residential_2010_parcel_inter.csv')
cmb <- cmb[,-c('OID_'),with=F]
cmb <- unique(cmb)
setnames(cmb,'FID_massbuilds_non_residential_2010_projects','massbuilds_ID')
cmb <- cmb[,.(identification,massbuilds_ID,name,street,city,notes,employment_capacity,start_year,yr_built,num_units,redevelopment,fy,ls_date)]
cnew <- cmb[yr_built>=2010]
czero <- cmb[yr_built==0 | is.na(yr_built)]
cold <- cmb[yr_built<2010 & yr_built!=0]
setorder(cold,-employment_capacity)
# Export for verification by massbuilds intern
# fwrite(czero,'massbuilds_nonresidential_builtYearZero.csv')
# fwrite(cnew,'massbuilds_nonresidential_built2010orAfter.csv')
# fwrite(cold,'massbuilds_nonresidential_before2010.csv')
# Create adjustment object for post-2010 projects
cnew[,newstart:=yr_built]
cnew.yr <- cnew[,.(identification,newstart)]
setkey(cnew.yr,identification)
c10 <- com[start_year==2010]
com <- com[start_year!=2010]
# Drop pre-2010 projects and but KEEP projects where built_year==0
c10 <- c10[!identification %in% cold$massbuilds_ID]
# Update start_year
setkey(c10,identification)
c10 <- cnew.yr[c10]
c10[!is.na(newstart),start_year:=newstart]
c10[,newstart:=NULL]
com <- rbind(c10,com)
com[city == 'Manchester', city := 'Manchester-by-the-Sea']


res[,tags:=paste0(status,'; mb_update_',vintage)]
com[,tags:=paste0(status,'; mb_update_',vintage)]


res[residential_units!=market_rate_units+affordable_units,`:=`(market_rate_units=residential_units,affordable_units=0)]

# Phase in Hopkinton larger developments earlier into the decade
res[city=='Hopkinton' & start_year>=2018 & start_year<=2020 & residential_units>5, `:=`(start_year=2013,duration=60)]

com[,redevelopment:='FALSE']
res[,redevelopment:='FALSE']
fwrite(res,paste0('MassBuilds/Massbuilds_inputs_',vintage,'/Statewide/residential.csv'))
fwrite(com,paste0('MassBuilds/Massbuilds_inputs_',vintage,'/Statewide/non_residential.csv'))

mres <- rbind(res,com,fill=T)
mres[,muni_name:=city]
setkey(mres,muni_name)

mc <- mapcdatakeys::all_muni_data_keys
setDT(mc)
mc <- mc[,.(muni_name,mapc)]
setkey(mc,muni_name)
mres <- mc[mres]
mres <- mres[mapc==1]
mres[,muni_name:=NULL][,mapc:=NULL]
mcom <- mres[is.na(residential_units)]
mres <- mres[!is.na(residential_units)]


fwrite(mres,paste0('MassBuilds/Massbuilds_inputs_',vintage,'/MAPC/residential.csv'))
fwrite(mcom,paste0('MassBuilds/Massbuilds_inputs_',vintage,'/MAPC/non_residential.csv'))

# Check against Census

dt <- res[start_year %in% 2011:2019,sum(residential_units),city]
setnames(dt,c('muni_name','mb_units'))
setkey(dt,muni_name)

munis <- mapcdatakeys::all_muni_data_keys
setDT(munis)
munis <- munis[,.(muni_name,cosub_cn10,cosub_cn20)]
setkey(munis,muni_name)

vac10 <-
  get_decennial(
    year = 2010,
    state = 'MA',
    geography = 'county subdivision',
    variables = 'H003001'
  ) %>%
  mutate(
    variable = case_when(
      variable == 'H003001' ~ "total_units_2010",
    )
  ) %>%
  pivot_wider(names_from = 'variable')
setDT(vac10)
vac10 <- vac10[total_units_2010!=0]
setnames(vac10,'GEOID','cosub_cn10')
setkey(vac10,cosub_cn10)
munis[,cosub_cn10:=as.character(cosub_cn10)]
setkey(munis,cosub_cn10)
vac10 <- munis[vac10]
vac10 <- vac10[,.(muni_name,total_units_2010)]
setkey(vac10, muni_name)
dt <- vac10[dt]

vac20 <-
  get_decennial(
    year = 2020,
    state = 'MA',
    geography = 'county subdivision',
    variables = c('H1_001N')
  ) %>%
  mutate(
    variable = case_when(
      variable == 'H1_001N' ~ "total_units_2020")) %>%
  pivot_wider(names_from = 'variable')
setDT(vac20)
vac20 <- vac20[total_units_2020!=0]
vac20 <- vac20[,.(GEOID,total_units_2020)]
setkey(vac20, GEOID)
munis[,GEOID:=as.character(cosub_cn20)]
setkey(munis, GEOID)
vac20 <- vac20[munis]
vac20 <- vac20[,.(muni_name,total_units_2020)]
setkey(vac20,muni_name)
dt <- vac20[dt]
dt[,cens_chg:=total_units_2020-total_units_2010]
dt[,pct_mb:=mb_units/cens_chg]
setorder(dt,-pct_mb)
setkey(dt,muni_name)

xl <- readxl::read_excel('developer_model_pause_munis_20221216.xlsx') %>% setDT()
setkey(xl,muni_name)
dt <- xl[dt]

mchk <- dt[pct_mb>1,.(muni_name,cens_chg)]
setnames(mchk,'muni_name','city')
setkey(mchk,city)

rchk <- res[start_year %in% 2011:2019 & city %in% sun(mchk$city)]
setkey(rchk,city)
rchk <- mchk[rchk]
setorder(rchk,city,start_year)

oc <- rchk[,.(city,start_year,identification,name,redevelopment,status,residential_units,cens_chg)]
oc[,frac:=residential_units/cens_chg]
oc[,frac_cumul:=cumsum(frac),city]


