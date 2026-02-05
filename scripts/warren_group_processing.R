setwd('S:/Network Shares/K Drive/DataServices/Datasets/Housing/Warren Group - Home Sales/Data/Tabular/Modified')
library(tidyverse)
library(data.table)
library(mapcdatakeys)
library(tidygeocoder)
sun <- function(x){sort(unique(x))}
lun <- function(x){length(unique(x))}

wr <- fread('20250703_warren_group_2000_2024_residential_final.csv')

to_geocode <- wr[lat<1]
to_geocode[,alt_add:=gsub(" #"," Unit ",geo_addr)]

results1 <- geo_combine(
  queries = list(
    list(method = 'census'), 
    list(method = 'osm'),
    list(method = 'arcgis')),
  address = to_geocode$geo_addr[1:10000],
  global_params = list(address = 'address'), 
  lat = lat,
  lon = lon
) |> 
  setDT()

results2 <- geo_combine(
  queries = list(
    list(method = 'census'), 
    list(method = 'osm'),
    list(method = 'arcgis')),
  address = to_geocode$geo_addr[10001:20000],
  global_params = list(address = 'address'), 
  lat = lat,
  lon = lon
) |> 
  setDT()

results3 <- geo_combine(
  queries = list(
    list(method = 'census'), 
    list(method = 'osm'),
    list(method = 'arcgis')),
  address = to_geocode$geo_addr[20001:dim(to_geocode)[1]],
  global_params = list(address = 'address'), 
  lat = lat,
  lon = lon
) |> 
  setDT()

results <- rbind(results1,results2,results3)

fwrite(results,'geocoded_NAs_20260205.csv')

pass_two <- results[is.na(lat),address]
if(dim(pass_two)[1]<10000 & dim(pass_two)[1]>0){
results4 <- pass_two |> 
  geo(method = 'google', lat = latitude , lon = longitude) %>% 
  setDT()
}

if(exists('results4')){results <- rbind(results,results4)}

setnames(results,c('geo_addr','lat_gc','lon_gc','gc_method'))
setkey(results, geo_addr)
setkey(wr, geo_addr)

wr.gc <- wr |>
  left_join(results, by = 'geo_addr') |>
  mutate(
    lat = case_when(lat == 0 ~ lat.gc, .default = lat),
    lon = case_when(lon == 0 ~ lon.gc, .default = lon)
  ) |> 
  select(-lat.gc,-lon.gc) |> 
  relocate(gc_method,.after = ct20_id)

wr.pts <- st_as_sf(wr.gc, coords=c('lon','lat'))


