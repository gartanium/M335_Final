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
      filter(!is.na(name)) %>%
      mutate(SC_ID_NAMED = paste(state_name, name, sep='_')) %>%
      select(SC_ID_NAMED, geometry)
  )
}

# Returns a dataframe containing the 2010 population for each county.
get_county_populations <- function() {
  return (
    us_cities() %>%
      filter(!is.na(population)) %>%
      mutate(SC_ID_NAMED = paste(state_name, county_name, sep='_')) %>%
      group_by(SC_ID_NAMED) %>%
      summarize(county_pop = sum(population))
  )
}

# Merges the population and county geometry data.
merge_county_data <- function() {
  geometry_dat <- get_county_geometries()
  pop_dat <- get_county_populations()
  return(
    dplyr::left_join(geometry_dat, pop_dat, by="SC_ID_NAMED") %>%
      filter(!is.na(county_pop))
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

get_merged_data <- function() {
  accident_code_dat <- merge_accident_code_dat()
  merged_county_dat <- merge_county_data()
  
  return(
    dplyr::left_join(accident_code_dat, merged_county_dat) %>%
      filter(!is.na(COUNTY_NAME))
  ) 
}

# Build our data
final_data <- get_merged_data()


