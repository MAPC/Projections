
library(data.table)
library(tidycensus)
library(mapcdatakeys)
library(tidyverse)
library(imputeTS)

# root <- 'K:/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/UrbanSim_Outputs/'
root <- 'S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/UrbanSim_Outputs/'

r2 <- 'S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2023/'

# root <- 'C:/Users/gately/Desktop/UrbanSim_scratch/'
comp.path <- paste0(root,'Run_Comparisons/')


# LRTP 2023 with zoning constraint fix and I-90 NO Build adjustments
MAPCM_run='run_196'
SWM_run='state_run_97'
ctps_run='run_97-196'


### LRTP 2023 Original Release Run Codes == (TDM 23.1.0 land use) - NO zoning constraint fix, so there are erroneous employment numbers in Boston, Chelsea, Revere, Somerville, etc.):

# MAPCM_run = 'run_139'
# SWM_run = 'state_run_97'

sun <- function(x){
  sort(unique(x))}
lun <- function(x){
  length(unique(x))}
sna <- function(x){
  sort(names(x))}

# List of RPA names
mpos <- unlist(c(mapcdatakeys::all_muni_data_keys |> select(mpo) |> unique()))

# Munis by RPA/MPO
munis <-
  mapcdatakeys::all_muni_data_keys |> select(muni_id, rpa_acr, mpo) |> setDT()

nonmapc <- munis[mpo!='MAPC']
# 2010 Blocks / Munis / RPA / MPO
blks <-
  mapcdatakeys::geog_xw_2010 |> left_join(munis, by = 'muni_id') |> select(bl10_id, muni_id, muni_name, rpa_acr, mpo) |> setDT()


mb <- fread('S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/MassBuilds/all_devprojects_8_19_2025.csv') |> 
  select(block_id, identification, project_id, name, address, start_year, building_type, building_type_id, residential_units, employment_capacity, status, tags)

mb.blk <- mb |> 
  group_by(block_id) |> 
  summarise(mb_units = sum(residential_units, na.rm=T))

m10 <- fread(paste0(root,MAPCM_run,'/results_mapc_',MAPCM_run,'_block_indicators_2010.csv'))
m50 <- fread(paste0(root,MAPCM_run,'/results_mapc_',MAPCM_run,'_block_indicators_2049.csv'))
s10 <- fread(paste0(root,SWM_run,'/results_mapc_',SWM_run,'_block_indicators_2010.csv')) |> 
  filter(!block_id %in% m10$block_id)
s50 <- fread(paste0(root,SWM_run,'/results_mapc_',SWM_run,'_block_indicators_2049.csv')) |> 
  filter(!block_id %in% m50$block_id)

d50 <- rbind(s50,m50,fill=T) |> 
  select(block_id,muni_id,total_households,total_residential_units,vacant_residential_units) |> 
  rename(hh2050=total_households,
         units2050=total_residential_units,
         vac2050=vacant_residential_units)

d10 <- rbind(s10,m10,fill=T) |> 
  select(block_id,muni_id,total_households,total_residential_units,vacant_residential_units,residential_unit_capacity) |> 
  rename(hh2010=total_households,
         units2010=total_residential_units,
         vac2010=vacant_residential_units,
         unit_capacity=residential_unit_capacity) |> 
  mutate(unit_capacity = round(unit_capacity))

 dt <- left_join(d10,d50, by=c('block_id','muni_id')) |> 
   left_join(mb.blk, by='block_id') |> 
   mutate(bl10_id = as.character(block_id),
          overbuild = units2050 - unit_capacity,
          mb_units = case_when(
            is.na(mb_units) ~ 0,
            .default=mb_units
          ),
          mb_plus_2010_units = units2010 + mb_units,
          mb_vs_2050_units = units2050 - mb_plus_2010_units
          )

started.over <- dt[units2010>unit_capacity]
gone.over <- dt[units2010<=unit_capacity & units2050>unit_capacity]
at.cap2050 <- dt[units2010<=unit_capacity & units2050==unit_capacity]
under2050 <- dt[units2050<unit_capacity]

gone.over[overbuild>mb_units]


sf <- mapcdatakeys::block_sf(2010) |> 
  left_join(dt, by=c('bl10_id','muni_id')) |> 
  select(-block_id)

st_write(sf, paste0(root,'lrtp_housing_units_and_zoning_capacity_block.shp'))
fwrite(dt, paste0(root,'lrtp_housing_units_and_zoning_capacity_block.csv'))

sw <- fread(paste0(r2,'Data/03_UrbanSim/Constraints/block_constraint_swm_zoning_inter.csv')) |> 
  mutate(DU = maxDUA_rev*sub_acres,
         bl10_id = as.character(bl10_id)) |> 
  group_by(bl10_id) |> 
  summarise(maxDU = sum(DU),
            acres = sum(sub_acres)) |> 
  right_join(dt, by='bl10_id')

cn <- fread(paste0(root,'urbansim_constraints_revised_20241011.csv'))
cn.row <- cn[,list( GEOID = unlist( strsplit( block_id_2010 , "," ) ) ) , by = uc_constraint_id ]
setkey(cn,uc_constraint_id)
setkey(cn.row,uc_constraint_id)
exp <- cn[cn.row]

er <- fread(paste0(r2,'Data/03_UrbanSim/Constraints/blocks2010_constraints_erased.csv')) |> 
  mutate(bl10_id=as.character(GEOID)) |> 
  select(bl10_id,area_acres) |> 
  right_join(sw, by='bl10_id') |> 
  mutate(acres = case_when(
    is.na(acres) & !is.na(area_acres) ~ area_acres,
    .default=acres
  ))

df <- exp |> select(GEOID,max_dua) |> 
  right_join(er,by=c('GEOID'='bl10_id')) |> 
  mutate(maxDU = case_when(
    is.na(maxDU) & !is.na(max_dua) ~ max_dua * acres,
    .default=maxDU
  )) |> 
  setDT()

df[GEOID %in% row & units2010>0 & units2050>units2010]
