

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ATTENTION!!
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## This script has not been updated to match definitions for "total vacant units"
## and "total available units" and "total existing units" as was done for the
## Statewide Housing Plan unit demand estimates - (See "RPA_unit_demand_calculator.qmd" 
## for more details)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## ATTENTION!!
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##



# Vacancy and Unit Demand post-model calculations
##
# Let RVRY = Regional Target Vacancy Rate for region R in year Y
# Avail_units_2020= hu_occ 2020 + (total_vacant_2020* (1-unavail_rate))
# Vacrate_avail_2020 = 1- ( hu_occ 2020/ Avail_units_2020)
# Calculate Housing Shortage/Surplus for 2020:
#   Hous_short_2020 = (RVR2020 - [vacrate_avail_2020]) * Avail_units_2020
# This may be positive (shortage) or negative (surplus)
#
# Calculate vacated units – number of units freed up due to HH decline
# VacatedHU_20_30 = Max(0, (-1 * HHchg_20_30))
#
# Calculate vacant units associated with HH growth
# HHgrowth_vac_20_30 =MAX(HHchg_20_30* RVR2030, 0)
#
# Discount production needed to close existing shortage
# Shortage_units_20_30 = IF (Hous_short_2020 > HHchg_20_30, MAX(HHchg_20_30, (Hous_short_2020 *0.5)), Hous_short_2020)
#
# Calculate total vacant units needed
# TotVac_20_30 = Shortage_units_20_30 + HHgrowth_vac_20_30
#
# Calculate total Housing Unit Demand
# Tot_HU_20_30 = max(max(HHchg_20_30, 0) + TotVac_20_30, 0)
#
# Calculate remaining shortfall for subsequent decade
# Hous_short_2030 = Hous_short_2020 – Shortage_units_20_30


# UrbanSim run code

mrun <- 'run_198'
srun <- 'state_run_113'

# dom <- 'MAPC'
# dom <- 'Statewide'
# run <- srun

urb_year <- 2019

# Citrix Files Path
setwd(
  'S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim'
)
output <- paste0(
  'S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/Unit_Demand/'
)

# Local K drive path
# setwd('K:/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/')
# output <-paste0('K:/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/Unit_Demand/')

# Output path for comparison maps
dir.create(output, showWarnings = F)

# Load packages
pacman::p_load(tidyverse,
               tidycensus,
               sf,
               leaflet,
               tigris,
               htmlwidgets,
               viridis,
               data.table)
# Enable tigris caching
options(tigris_use_cache = TRUE)
# Disabling Scientific Notation
options(scipen = 999)


# RVR-based unit demand

########
# NOTE --- data for the towns of Hanover, Duxbury, Pembroke, and Stoughton
# is taken from the SWM runs of UrbanSim, not from the MAPCM runs
########
 

# Set residential vacancy rate targets
# Load in the actual 2020 unit counts and vacancy rates from Census 2020 DHC

munis <- mapcdatakeys::all_muni_data_keys %>%
  select(muni_id, muni_name, cosub_cn20, cmtyp08, mapc, mpo, rpa_acr) %>%
  mutate(GEOID = as.character(cosub_cn20)) %>%
  mutate(
    rvr = case_when(
      cmtyp08 == 'Inner Core' ~ 0.05,
      cmtyp08 == 'Developing Suburb' ~ 0.04,
      cmtyp08 == 'Maturing Suburb' ~ 0.04,
      cmtyp08 == 'Regional Urban Center' ~ 0.05,
      cmtyp08 == 'Rural Town' ~ 0.04,
      TRUE ~ 0.04
    )
  )

vars <- load_variables(2020, 'dhc') %>% setDT()
vs <- vars[grepl('VACANCY', concept), name]
ns <- vars[grepl('VACANCY', concept), gsub(':', '', gsub(' ', '_', (gsub(
  ',', '', gsub(':!!', '_', (gsub(' !!', '', (
    label
  ))))
))))]
vs <- c('H1_001N', 'P20_001N', vs)
d1 <-
  get_decennial(
    year = 2020,
    sumfile = 'dhc',
    state = 'MA',
    geography = 'county subdivision',
    variables = vs
  ) %>%
  pivot_wider(names_from = 'variable') %>%
  select(-NAME) %>%
  setDT()
setnames(d1, c('GEOID', 'dhc_units_2020', 'dhc_households_2020', ns))

m2 <- munis %>% select(GEOID, muni_id)
dhc <- d1 %>%
  left_join(m2, by = 'GEOID') %>%
  mutate(vac_avail_2020 = Total_For_sale_only + Total_For_rent)

pl94 <-
  get_decennial(
    year = 2020,
    state = 'MA',
    geography = 'county subdivision',
    variables = c('H1_001N', 'H1_002N', 'H1_003N')
  ) %>%
  mutate(
    variable = case_when(
      variable == 'H1_001N' ~ "total_units_2020",
      variable == 'H1_002N' ~ "total_occ_2020",
      variable == 'H1_003N' ~ "total_vacant_2020"
    )
  ) %>%
  pivot_wider(names_from = 'variable') %>% left_join(munis, by = 'GEOID') %>%
  select(
    muni_id,
    muni_name,
    mpo,
    rpa_acr,
    cmtyp08,
    rvr,
    total_units_2020,
    total_occ_2020,
    total_vacant_2020
  ) %>%
  setDT()


fload <- function(y) {
  dt <- fread(
    paste0(
      'UrbanSim_Outputs/',
      mrun,
      '/results_mapc_',
      mrun,
      '_muni_indicators_',
      y,
      '.csv'
    )
  ) %>% 
    filter(!muni_id %in% c(82,122,231,285)) %>% 
    mutate(model = 'MAPCM',
           run = mrun) %>% 
    relocate(run, .before = 'muni_id') %>% 
    relocate(model, .before = 'run')

  sdt <- fread(
    paste0(
      'UrbanSim_Outputs/',
      srun,
      '/results_mapc_',
      srun,
      '_muni_indicators_',
      y,
      '.csv'
    )
  ) %>%
    filter(!muni_id %in% dt$muni_id) %>%
    mutate(model = 'SWM',
           run = srun) %>% 
    relocate(run, .before = 'muni_id') %>% 
    relocate(model, .before = 'run') %>% 
    rbind(dt, fill = T) %>%
    mutate(year = y + 1) %>%
    select(
      year,
      run,
      model,
      muni_id,
      total_households,
      total_residential_units,
      sum_vacant_residential_units
    ) %>%
    
    left_join(pl94, by = 'muni_id') %>%
    # left_join(dhc, by = 'muni_id') %>%
    mutate(
      avail_units = total_households + sum_vacant_residential_units,
      avail_vacrate = 1 - (total_households / avail_units),
      units_short = (rvr - avail_vacrate) * avail_units,
    )
}

tl <- lapply(list(2010, 2019, 2029, 2039, 2049), fload)
urb <- rbindlist(tl)
setDT(urb)
urb[year == 2011, year := 2010]
setorder(urb, muni_id, year)

uw <- dcast.data.table(
  urb,
  muni_id + muni_name + rvr + cmtyp08 ~ year,
  value.var = c(
    'total_households',
    'total_residential_units',
    'sum_vacant_residential_units',
    'avail_units',
    'avail_vacrate',
    'units_short'
  )
)
uw[, HHchg_20_30 := total_households_2030 - total_households_2020]
uw[, HHchg_30_40 := total_households_2040 - total_households_2030]
uw[, HHchg_40_50 := total_households_2050 - total_households_2040]

# 2020 to 2030
uw[, VacatedHU_20_30 := min(0, HHchg_20_30), muni_id]
uw[, HHgrowth_vac_20_30 := HHchg_20_30 * rvr, muni_id]
uw[units_short_2020 > HHchg_20_30, Shortage_units_20_30 := max(HHchg_20_30, (units_short_2020 *
                                                                               0.5)), muni_id]
uw[units_short_2020 <= HHchg_20_30, Shortage_units_20_30 := units_short_2020]
uw[, Vacant_Units_Needed_20_30 := Shortage_units_20_30 + HHgrowth_vac_20_30]
uw[, Total_Housing_Unit_Demand_20_30 := max(max(HHchg_20_30, 0) + Vacant_Units_Needed_20_30, 0), muni_id]
uw[, units_short_2030 := units_short_2020 - Shortage_units_20_30]

# 2030 to 2040
uw[, VacatedHU_30_40 := max(0, (HHchg_30_40) * -1), muni_id]
uw[, HHgrowth_vac_30_40 := max(HHchg_30_40 * rvr, 0), muni_id]
uw[units_short_2030 > HHchg_30_40, Shortage_units_30_40 := max(HHchg_30_40, (units_short_2030 *
                                                                               0.5)), muni_id]
uw[units_short_2030 <= HHchg_30_40, Shortage_units_30_40 := units_short_2030]
uw[, Vacant_Units_Needed_30_40 := Shortage_units_30_40 + HHgrowth_vac_30_40]
uw[, Total_Housing_Unit_Demand_30_40 := max(max(HHchg_30_40, 0) + Vacant_Units_Needed_30_40, 0), muni_id]
uw[, units_short_2040 := units_short_2030 - Shortage_units_30_40]

# 2040 to 2050
uw[, VacatedHU_40_50 := max(0, (HHchg_40_50) * -1), muni_id]
uw[, HHgrowth_vac_40_50 := max(HHchg_40_50 * rvr, 0), muni_id]
uw[units_short_2040 > HHchg_40_50, Shortage_units_40_50 := max(HHchg_40_50, (units_short_2040 *
                                                                               0.5)), muni_id]
uw[units_short_2040 <= HHchg_40_50, Shortage_units_40_50 := units_short_2040]
uw[, Vacant_Units_Needed_40_50 := Shortage_units_40_50 + HHgrowth_vac_40_50]
uw[, Total_Housing_Unit_Demand_40_50 := max(max(HHchg_40_50, 0) + Vacant_Units_Needed_40_50, 0), muni_id]
uw[, units_short_2050 := units_short_2040 - Shortage_units_40_50]
uw <- uw %>% relocate(
  Total_Housing_Unit_Demand_20_30,
  Total_Housing_Unit_Demand_30_40,
  Total_Housing_Unit_Demand_40_50,
  .after = total_residential_units_2050
)


mpo <- munis %>% select(muni_id, mpo, rpa_acr) %>% setDT()
setkey(uw, muni_id)
setkey(mpo, muni_id)
m1 <- mpo[uw]

fwrite(m1, paste0(output, mrun, '_', srun, '_RVR_target_unit_demand_estimates.csv'))

#### Latent demand by municipality #### 

yr <- 2019
# Prep population files:

dt <- readRDS(paste0(
  'UrbanSim_Outputs/',mrun,'/urbansim_',mrun,'_microhouseholds_',yr,'_pop_corrected_v3.rds')) %>%
  filter(mpo == 'MAPC')

st <- readRDS(paste0(
  'UrbanSim_Outputs/',srun,'/urbansim_',srun,'_microhouseholds_',yr,'_pop_corrected_v3.rds')) %>% 
  filter(!mpo %in% c('MAPC'))

dt <- rbind(dt, st)

spop <- dt[, .N, ageCAT6]
setorder(spop, ageCAT6)
fwrite(spop, paste0('Unit_Demand/state_HHpop_by_ageHHder_',yr,'.csv'))

mpop <- dt[, .N, .(muni_id, ageCAT6)]
setorder(mpop, ageCAT6)
fwrite(mpop, paste0('Unit_Demand/muni_HHpop_by_ageHHder_',yr,'.csv'))


m1 <- fread(paste0(
  output,mrun,'_',srun,'_implied_unit_demand_estimates.csv'
))

hhr <- fread('Unit_Demand/hh_rates_2000_1519.csv')

mc <- fread('Unit_Demand/muni_HHpop_by_ageHHder_2019.csv') %>%
  filter(ageCAT6 > 3) %>%
  rename(HHpop = N) %>%
  left_join(hhr, by = 'ageCAT6') %>%
  mutate(
    hh2000 = HHpop * census2000,
    hh1519 = HHpop * acs1519,
    hhdiff = hh2000 - hh1519,
    hh2000_nochild = HHpop * head_nochild2000,
    hh2000_child = HHpop * head_child2000,
    hh2000_single = HHpop * head_single2000,
    hh1519_nochild = HHpop * head_nochild1519,
    hh1519_child = HHpop * head_child1519,
    hh1519_single = HHpop * head_single1519,
    hhdiff_nochild = hh1519_nochild - hh2000_nochild,
    hhdiff_child = hh1519_child - hh2000_child,
    hhdiff_single = hh1519_single - hh2000_single,
  )
mc[ageCAT6 %in% 4:9, lat_single := abs(sum(hhdiff_single)), muni_id]
mc[ageCAT6 %in% 4:6, lat_nochild := abs(sum(hhdiff_nochild)), muni_id]
mc[, lat_pub := abs(min(lat_single, na.rm = T) + min(lat_nochild, na.rm =
                                                       T)), muni_id]

latent_public <- mc %>%
  select(muni_id, lat_pub) %>%
  unique() %>%
  mutate(latent_2030 = round(lat_pub * 0.52),
         latent_2050 = round(lat_pub * 0.48)) %>%
  select(muni_id, latent_2030, latent_2050) %>%
  setDT()
setkey(latent_public, muni_id)

dmuni <- m1[, lapply(.SD, sum, na.rm = T), .(muni_id, muni_name, mpo), .SDcols =
              c(
                'total_households_2020',
                'total_households_2030',
                'total_households_2050',
                'total_residential_units_2020',
                'Total_Housing_Unit_Demand_20_30',
                'Total_Housing_Unit_Demand_30_40',
                'Total_Housing_Unit_Demand_40_50'
              )]
setnames(
  dmuni,
  c(
    'muni_id',
    'muni_name',
    'mpo',
    'hh2020',
    'hh2030',
    'hh2050',
    'existing_units_2020',
    'unit_demand_2030',
    'unit_demand_2040',
    'unit_demand_2050'
  )
)

setkey(dmuni, muni_id)

ds <- latent_public[dmuni]

ds[, total_unit_demand_2030 := round(unit_demand_2030 + latent_2030)]
ds[, total_unit_demand_2050 := round(unit_demand_2030 + unit_demand_2040 + unit_demand_2050 + latent_2030 + latent_2050)]
ds[, vac_2020 := 1 - hh2020 / existing_units_2020]
ds[, vac_2030 := 1 - hh2030 / (existing_units_2020 + total_unit_demand_2030)]

fwrite(ds,
       'Unit_Demand/muni_latent_unit_demand_public_version_ageCAT.csv')

# Include all households under ageCAT 11 in the latent demand calculations
latent_total_const <-
  round(abs(mc[ageCAT6 < 10, sum(hhdiff_nochild) + sum(hhdiff_child) +
                 sum(hhdiff_single)]))

latent_comp <- mc[ageCAT6 < 10, round(abs(sum(hhdiff_nochild) + sum(hhdiff_child) +
                                            sum(hhdiff_single))), muni_id] %>%
  unique() %>%
  mutate(latent_2030 = round(V1 * 0.52),
         latent_2050 = round(V1 * 0.48)) %>%
  select(muni_id, latent_2030, latent_2050) %>%
  setDT()
setkey(latent_comp, muni_id)


dc <- latent_comp[dmuni]

dc[, total_unit_demand_2030 := round(unit_demand_2030 + latent_2030)]
dc[, total_unit_demand_2050 := round(unit_demand_2030 + unit_demand_2040 + unit_demand_2050 + latent_2030 + latent_2050)]
dc[, vac_2020 := 1 - hh2020 / existing_units_2020]
dc[, vac_2030 := 1 - hh2030 / (existing_units_2020 + total_unit_demand_2030)]

fwrite(dc,
       'Unit_Demand/muni_latent_unit_demand_comprehensive_ageCAT.csv')


# Previous latent demand subset versions

# blks <- mapcdatakeys::geog_xw_2010 %>%
#   select(bl10_id, county) %>%
#   filter(
#     county %in% c(
#       'Essex County',
#       'Middlesex County',
#       'Norfolk County',
#       'Suffolk County',
#       'Plymouth County'
#     )
#   )
# 
# dt <- readRDS(
#   'UrbanSim_Outputs/run_139/urbansim_run_139_microhouseholds_2019_pop_corrected_v3.rds'
# ) %>% filter(mpo == 'MAPC')
# st <- readRDS(
#   'UrbanSim_Outputs/state_run_97/urbansim_state_run_97_microhouseholds_2019_pop_corrected_v3.rds'
# ) %>% filter(!mpo %in% c('MAPC'))
# 
# dt <- rbind(dt, st)
# 
# spop <- dt[, .N, ageCAT6]
# setorder(spop, ageCAT6)
# fwrite(spop, 'Unit_Demand/state_HHpop_by_ageHHder_2019.csv')
# 
# mpop <- dt[, .N, .(muni_id, ageCAT6)]
# setorder(mpop, ageCAT6)
# fwrite(mpop, 'Unit_Demand/muni_HHpop_by_ageHHder_2019.csv')
# 
# d5 <- dt[bl10_id %in% blks$bl10_id]
# dpop <- d5[, .N, ageCAT6]
# setorder(dpop, ageCAT6)
# fwrite(dpop, 'Unit_Demand/five_county_HHpop_by_ageHHder_2019.csv')
# 
# mp <- readRDS(
#   'UrbanSim_Outputs/run_139/urbansim_run_139_microhouseholds_2019_pop_corrected_v3.rds'
# )
# dmpop <- mp[, .N, ageCAT6]
# setorder(dmpop, ageCAT6)
# fwrite(dmpop, 'Unit_Demand/MAPC101_HHpop_by_ageHHder_2019.csv')
# 

# 
# # Statewide latent demand
# latent.st <- 52227
# state_units <- m1[, lapply(.SD, sum, na.rm = T), .SDcols = c(
#   'Total_Housing_Unit_Demand_20_30',
#   'Total_Housing_Unit_Demand_30_40',
#   'Total_Housing_Unit_Demand_40_50'
# )]
# s2030 <- state_units[, Total_Housing_Unit_Demand_20_30 + 27214]
# s2050 <- state_units[, Total_Housing_Unit_Demand_20_30 + Total_Housing_Unit_Demand_30_40 +
#                        Total_Housing_Unit_Demand_40_50 + latent.st]
# c(s2030, s2050)
# 
# # Five county latent demand
# latent.5c <- 35225
# m5 <- m1[muni_id %in% unique(dt$muni_id)]
# s5 <- m5[, lapply(.SD, sum, na.rm = T), .SDcols = c(
#   'Total_Housing_Unit_Demand_20_30',
#   'Total_Housing_Unit_Demand_30_40',
#   'Total_Housing_Unit_Demand_40_50'
# )]
# f2030 <- s5[, Total_Housing_Unit_Demand_20_30 + (latent.5c * .52)]
# f2050 <- s5[, Total_Housing_Unit_Demand_20_30 + Total_Housing_Unit_Demand_30_40 +
#               Total_Housing_Unit_Demand_40_50 + latent.5c]
# c(f2030, f2050)
# 
# # MAPC 101
# 
# m101 <- fread(paste0(output, mrun, '_implied_unit_demand_estimates.csv'))
# 
# latent.101 <- 27162
# s5 <- m101[, lapply(.SD, sum, na.rm = T), .SDcols = c(
#   'Total_Housing_Unit_Demand_20_30',
#   'Total_Housing_Unit_Demand_30_40',
#   'Total_Housing_Unit_Demand_40_50'
# )]
# f2030 <- s5[, Total_Housing_Unit_Demand_20_30 + (latent.101 * .52)]
# f2050 <- s5[, Total_Housing_Unit_Demand_20_30 + Total_Housing_Unit_Demand_30_40 +
#               Total_Housing_Unit_Demand_40_50 + latent.101]
# c(f2030, f2050)


