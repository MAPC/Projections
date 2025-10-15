# Queries the Census API to retrieve PUMS data.
# var.list: list of variables to include in the data query.
# yr: end year of the survey.
# srvy: one year or five year PUMS. Use either "acs1" or "acs5".
pums_query <- function(var.list, yr, srvy){
  get_pums(
    variables = var.list,
    state = "MA",
    year = yr,
    survey = srvy
  )
}

# Various helper functions for sorting unique data
sun <- function(x){
  sort(unique(x))}
lun <- function(x){
  length(unique(x))}
sna <- function(x){
  sort(names(x))}