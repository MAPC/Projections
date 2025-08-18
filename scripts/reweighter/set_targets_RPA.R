
tmp <- fread(paste0(workpath,'Templates/AgeHHder_HHtype_template.csv'))

mtp <- tmp[RPA == mid]

target <- pums %>%
  filter(SPORDER == 1) %>%
  group_by(AgeCat, HHtype) %>%
  summarise(BASELINE = sum(WGTP)) %>%
  mutate(
    AgeCat = case_when(
      AgeCat == 1 ~ "x == 1",
      AgeCat == 2 ~ "x == 2",
      AgeCat == 3 ~ "x == 3",
      AgeCat == 4 ~ "x == 4",
      AgeCat == 5 ~ "x == 5",
      AgeCat == 6 ~ "x == 6",
      AgeCat == 7 ~ "x == 7",
      AgeCat == 8 ~ "x == 8",
      AgeCat == 9 ~ "x == 9",
      AgeCat == 10 ~ "x == 10",
      AgeCat == 11 ~ "x == 11",
      AgeCat == 12 ~ "x == 12",
      AgeCat == 13 ~ "x == 13",
      AgeCat == 14 ~ "x == 14",
      AgeCat == 15 ~ "x == 15",
      AgeCat == 16 ~ "x == 16",
      AgeCat == 17 ~ "x == 17",
      AgeCat == 18 ~ "x == 18"
    ),
    HHtype = case_when(HHtype == 1 ~ "x == 1",
                       HHtype == 3 ~ "x == 3",
                       HHtype == 5 ~ "x == 5"),
    RPA = mid
  ) %>%
  right_join(mtp, by = c('RPA', 'AgeCat', 'HHtype')) %>%
  left_join(thh, by = c('RPA', 'AgeCat', 'HHtype')) %>%
  setDT()

target[is.na(BASELINE), BASELINE := 0]
target[is.na(TARGET), TARGET := 0]
target[, INTER := BASELINE]
target[, RPA := NULL]
setorder(target,AgeCat,HHtype)
write.csv(target, paste0(mid, '/AgeHHder_HHtype.csv'))
