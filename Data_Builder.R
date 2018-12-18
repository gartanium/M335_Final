library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(USAboundaries)
library(maps)
#library(sf)
#library(sf)

# Gets the national data for every car crash involving a fatality.
get_accident_data <- function() {
  return(
    dat <- read_csv("Data/Accidents_Data.csv") %>%
      rename(COUNTY_NUMERIC = county) %>%
      mutate(STATE_ABV = state.abb[match(state_name,state.name)]) %>%
      mutate(SC_ID= paste(STATE_ABV, COUNTY_NUMERIC, sep='_')) %>%
      filter(state_name == "Idaho") %>%
      select( -COUNTY_NUMERIC)
  )
}

write_data <- function(dat) {
  dat %>%
    select(-geometry) %>%
    write.csv("Final_Data.csv")
}

# Gets the GLA data for county codes.
get_code_data <- function() {
  return(
    dat <- read_csv("Data/NationalFedCodes.csv") %>%
      select(COUNTY_NAME, STATE_ALPHA, COUNTY_NUMERIC) %>%
      group_by(COUNTY_NUMERIC, STATE_ALPHA) %>%
      filter(STATE_ALPHA == "ID") %>%
      filter(row_number(COUNTY_NAME) == 1) %>%
      ungroup() %>%
      mutate(SC_ID= paste(STATE_ALPHA, COUNTY_NUMERIC, sep='_')) %>%
      select(-STATE_ALPHA, -COUNTY_NUMERIC)
  )
}

# Returns a dataset containing the geometries of every county.
get_county_geometries <- function() {
  return (
    USAboundaries::us_counties() %>%
      filter(state_name == "Idaho") %>%
      filter(!is.na(name)) %>%
      mutate(SC_ID_NAMED = paste(state_name, name, sep='_')) %>%
      select(SC_ID_NAMED, geometry)
  )
}


# Returns a dataframe containing the 2010 population for each county.
get_county_populations <- function() {
  
  pop_dat <- read_csv("Data/County_Populations.csv")
  return (
    pop_dat %>%
      group_by(county_name) %>%
      mutate(SC_ID_NAMED = paste("Idaho", county_name, sep='_'))
  )
}

# Merges the population and county geometry data.
merge_county_data <- function() {
  geometry_dat <- get_county_geometries()
  pop_dat <- get_county_populations()
  return(
    dplyr::left_join(geometry_dat, pop_dat, by="SC_ID_NAMED") 
  )
}

merge_accident_code_dat <- function() {
  accident_dat <- get_accident_data() 
  code_dat <- get_code_data()
  return(
    dplyr::left_join(accident_dat, code_dat) %>%
      mutate(SC_ID_NAMED = paste(state_name, COUNTY_NAME, sep='_'))
  )
}

View(merge_accident_code_dat())

get_merged_data <- function() {
  accident_code_dat <- merge_accident_code_dat() 
  merged_county_dat <- merge_county_data()
  centeroids <- read_csv("Data/Idaho_Geo_County_Centeroids.csv") %>%
    mutate(county_latitude = as.numeric(county_latitude)) %>%
    mutate(county_longitude = as.numeric(county_longitude) * -1)
  
  accident_code_dat <- dplyr::left_join(centeroids, accident_code_dat)
   
  return(
    dplyr::left_join(accident_code_dat, merged_county_dat) %>%
      filter(!is.na(COUNTY_NAME)) 
  ) 
}

# Build our data
final_data <- get_merged_data()

