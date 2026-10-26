rw <- readRDS("S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2028/Data/01_Regional_Control_Totals/SHoP_Scenarios/Reweighter_Outputs/Scenario_3/processor_corrected_PUMS_MAPC97_Scenario_3_with_ShelterPop_2050.rds")
rw <- fread("S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/Scenario_3/Output_Files_UPDATE/reweighted_PUMS_Scenario_3_MAPC101_scen2_2035.csv")
cc <- readRDS("S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/Scenario_3/Output_Files_UPDATE/processor_corrected_PUMS_MAPC101_Scenario_3_2035.rds")
sh <- readRDS("S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/Scenario_3/Output_Files_UPDATE/processor_corrected_PUMS_MAPC101_Scenario_3_with_ShelterPop_2035.rds")

setnames(rw,'rpa_acr','mpo')
setnames(cc,'rpa_acr','mpo')
setnames(sh,'rpa_acr','mpo')
rws <- rw[SPORDER==1, round(sum(new_WGTP)),mpo]
ccs <- cc[SPORDER==1, round(sum(new_WGTP)),mpo]
shs <- sh[SPORDER==1, round(sum(new_WGTP)),mpo]
setnames(rws,c('mpo','reweighter_hh_2035'))
setnames(ccs,c('mpo','pop_corrected_hh_2035'))
setnames(shs,c('mpo','pop_corrected_with_shelter_hh_2035'))
exc <- left_join(rws,ccs,by='mpo') |> left_join(shs,by='mpo') |> setDT()
exc[,diff1:=pop_corrected_hh_2035-reweighter_hh_2035]
exc[,diff2:=pop_corrected_hh_2035-reweighter_hh_2035]
setorder(exc,mpo)

