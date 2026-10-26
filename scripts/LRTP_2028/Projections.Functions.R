# Sorting and Length Functions
sun <- function(x){
  sort(unique(x))}
lun <- function(x){
  length(unique(x))}
sna <- function(x){
  sort(names(x))}

#decennial_call - Decennial Census Data API Query
#decennial.var_name: list of variables to query the API
#year: vintage of decennial census
decennial_call <- function(decennial.var_name, geog, year){
  if (year == 2010){
    get_decennial(
      variables = decennial.var_name,
      geography = geog, #Specify the level of geography
      state = "MA", #Only evaluating MA municipalities
      year = year, #Set the year in the function call (either 2010 or 2020)
      sumfile = "sf1" #Different summary files depending on year
    )
    
  } else{
    get_decennial(
      variables = decennial.var_name,
      geography = geog, #Specify the level of geography
      state = "MA", #Only evaluating MA municipalities
      year = year, #Set the year in the function call (either 2010 or 2020)
      sumfile = "dhc" #Different summary files depending on year
    )
  }
}

#Decennial Data Cleaning Functions
#dec.vars - loads a list of variables to pass to the decennial census query
#vars: dataframe of all variables in decennial census. pick from vars.dec10 and vars.dec20
#concept: string of the "concept" variable in the vars dataframe
dec.vars <- function(vars, con){
  vars |>  
    filter(concept == con) |> 
    select(name) |> 
    mutate(
      name = as.character(name)
    ) |>  
    pull(name) 
}

#dec.labels - Attaches relevant data labels to data pulled from the decennial census
#by five-year age group
#year: dataframe of all variables in decennial census. pick from vars.dec10 and vars.dec20
#con: string of the "concept" variable in the vars dataframe
dec.labels <- function(year, con){
  if (year == "2010"){
    vars.dec10 |>  
      filter(concept == con) |> #Filter the data to the specific concept
      select(name, label) |>  #Select relevant variables
      filter(str_detect(label, "years")) |> #Removing overhead categories
      #Mutate the text of the label variable to make easier cross-reference to ageCAT6
      #age groups later on in the analysis
      mutate( 
        sex1 = str_extract(label, "Female"),
        sex2 = str_extract(label, "Male"),
        sex = coalesce(sex1,sex2),
        label = gsub("Total!!Male!!","", label),
        label = gsub("Total!!Female!!","", label),
        label = gsub(" ", "_", label),
        label = paste0("pop_", label)
      ) |> 
      #Remove superfluous variables used to coalesce
      select(-c(sex1, sex2)) |>  
      na.omit()
    
  } else{
    vars.dec20 |>  
      filter(concept == con) |> #Filter the data to the specific concept
      select(name, label) |>  #Select relevant variables
      filter(str_detect(label, "years")) |> #Removing overhead categories
      #Mutate the text of the label variable to make easier cross-reference to ageCAT6
      #age groups later on in the analysis
      mutate(
        sex1 = str_extract(label, "Female"),
        sex2 = str_extract(label, "Male"),
        sex = coalesce(sex1,sex2),
        label = gsub("!!Total:!!Male:","", label),
        label = gsub("!!Total:!!Female:","", label),
        label = gsub(" ", "_", label),
        label = sub("!!", "", label),
        label = paste0("pop", label),
      ) |> 
      #Remove superfluous variables used to coalesce
      select(-c(sex1, sex2)) |>  
      na.omit()
    
  }
}

#Group Quarters Cleaning function
gq.clean <- function(df){
  df |> 
    # Mutate the text of the label variable to make easier cross-reference to ageCAT6
    # age groups later on in the analysis
    mutate(
      label = str_replace(label,"\\s*\\([^\\)]+\\)",""),
      label = str_replace(label, "Total_!!Male!!",""),
      label = str_replace(label, "Total_!!Female!!",""),
      label = str_replace(label, "Total_:!!Male:!!",""),
      label = str_replace(label, "Total_:!!Female:!!","")
    )
}