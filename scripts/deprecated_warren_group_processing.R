setwd('S:/Network Shares/K Drive/DataServices/Datasets/Housing/Warren Group - Home Sales/Data/Tabular/Modified/Processed/archive')
library(tidyverse)
library(data.table)
library(sf)
library(mapcdatakeys)
library(tidygeocoder)
sun <- function(x){sort(unique(x))}
lun <- function(x){length(unique(x))}

df24 <- fread(paste0('./Processed/archive/warren_group_2000_', data_year,'_final.csv'))
df25 <- fread('./Preprocessing/warren_group_2025_final.csv')

df <- rbind(df24,df25, fill=T)
rm(df24,df25);gc()

results <- fread('../geocoded_NAs_20260205.csv')
setnames(results,c('geo_addr','lat_gc','lon_gc','gc_method'))

dt <- df |>
  left_join(results, by = 'geo_addr') |>
  mutate(
    lat = case_when(lat == 0 ~ lat_gc, .default = lat),
    lon = case_when(lon == 0 ~ lon_gc, .default = lon)
  ) |>
  select(-lat_gc,-lon_gc) |>
  relocate(gc_method,.after = ct20_id) |> 
  setDT()

to.gc <- dt |> 
  filter(lat<1) |> 
  pull(geo_addr) |> 
  unique()

results1 <- geo_combine(
  queries = list(
    list(method = 'arcgis'),
    list(method = 'osm'),
    list(method = 'census')),
  address = to_geocode$geo_addr[1:10000],
  global_params = list(address = 'address'), 
  lat = lat,
  lon = lon
) |> 
  setDT()

wr.pts <- st_as_sf(wr.gc, coords=c('lon','lat'))
st_crs(wr.pts) <- 4326
wr.stpln <- st_transform(wr.pts, 26986)

to_geocode <- wr[lat<1]
to_geocode[,alt_add:=gsub(" #"," Unit ",geo_addr)]

results1 <- geo_combine(
  queries = list(
    list(method = 'arcgis'),
    list(method = 'osm'),
    list(method = 'census')),
  address = to_geocode$geo_addr[1:10000],
  global_params = list(address = 'address'), 
  lat = lat,
  lon = lon
) |> 
  setDT()

results2 <- geo_combine(
  queries = list(
    list(method = 'arcgis'),
    list(method = 'osm'),
    list(method = 'census')),
  address = to_geocode$geo_addr[10001:20000],
  global_params = list(address = 'address'), 
  lat = lat,
  lon = lon
) |> 
  setDT()

results3 <- geo_combine(
  queries = list(
    list(method = 'arcgis'),
    list(method = 'osm'),
    list(method = 'census')),
  address = to_geocode$geo_addr[20001:dim(to_geocode)[1]],
  global_params = list(address = 'address'), 
  lat = lat,
  lon = lon
) |> 
  setDT()

results <- rbind(results1,results2,results3) |> unique()

fwrite(results,'../../geocoded_NAs_20260205.csv')
results <- fread('../../geocoded_NAs_20260205.csv')

# pass_two <- results[is.na(lat),address]
# if(dim(pass_two)[1]<10000 & dim(pass_two)[1]>0){
# results4 <- pass_two |> 
#   geo(method = 'google', lat = latitude , lon = longitude) %>% 
#   setDT()
# }
# 
# if(exists('results4')){results <- rbind(results,results4) |> unique()}


setnames(results,c('geo_addr','lat_gc','lon_gc','gc_method'))

wr.gc <- wr |>
  left_join(results, by = 'geo_addr') |>
  mutate(
    lat = case_when(lat == 0 ~ lat_gc, .default = lat),
    lon = case_when(lon == 0 ~ lon_gc, .default = lon)
  ) |> 
  select(-lat_gc,-lon_gc) |> 
  relocate(gc_method,.after = ct20_id)

wr.pts <- st_as_sf(wr.gc, coords=c('lon','lat'))
st_crs(wr.pts) <- 4326
wr.stpln <- st_transform(wr.pts, 26986)

saveRDS(wr.stpln, 'warren_group_2000_2024_residential_final_sf.rds')
st_write(wr.stpln, 'warren_group_2000_2024_residential_final_sf.gpkg')
