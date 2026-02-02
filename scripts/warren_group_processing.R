setwd('S:/Network Shares/K Drive/DataServices/Datasets/Housing/Warren Group - Home Sales/Data/Tabular/Modified')
library(tidyverse)
library(data.table)
library(mapcdatakeys)

wr <- fread('20250703_warren_group_2000_2024_residential_final.csv')
