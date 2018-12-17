library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(USAboundaries)
library(leaflet)
library(maps)

display_crashes_by_state <- function(dat) {
  merged_data <- dat 
  
  ggplot()+
    geom_sf(data = merged_data, aes(fill = county_pop)) +
    scale_fill_gradient2()
}

display_fatalaties_by_county <- function(dat) {
  merged_data <- dat %>%
    filter(state_name == "California") %>%
    filter(county_pop >= 100000) %>%
    group_by(COUNTY_NAME) %>%
    mutate(total_fatalities = sum(number_of_fatalities)) %>%
    filter(row_number()==1) %>%
    mutate(total_fatalities = total_fatalities*100000/county_pop) %>%
    select(COUNTY_NAME, total_fatalities, county_pop, geometry) 
  
  ggplot()+
    geom_sf(data = merged_data, aes(fill = total_fatalities)) +
    scale_fill_gradient2()
}

leaflet_display_fatalaties_by_county <- function(dat) {
  merged_data <- dat %>%
    filter(state_name == "Idaho") %>%
    filter(county_pop >= 0) %>%
    group_by(COUNTY_NAME) %>%
    mutate(total_fatalities = sum(number_of_fatalities)) %>%
    filter(row_number()==1) %>%
    mutate(total_fatalities = total_fatalities*100000/county_pop) %>%
    select(COUNTY_NAME, total_fatalities, county_pop, geometry) %>%
    ungroup() %>% 
    as_tibble() 
  
  library(sf) 
  merged_data <- merged_data %>%
    st_as_sf()
  
  
  pal <- colorNumeric(c("red", "green", "blue"), min(merged_data$total_fatalities):max(merged_data$total_fatalities))
  
  leaflet(data=merged_data) %>% 
    addTiles() %>%
    addPolygons(data=merged_data, weight=.45, group = t, fillColor = ~pal(merged_data$total_fatalities)) %>%
    setView(lng=-112.033966, lat=43.491650, zoom=7) %>%
    addMarkers(~Long, ~Lat, label = ~htmlEscape(Name))    
}

leaflet_display_fatalaties_by_county(dat)



display_Idaho_crashes <- function(dat) {
  merged_data <- dat %>%
    filter(STATE_ABV == "ID")
  ggplot()+
    geom_sf(data = merged_data, aes(fill = county_pop)) +
    scale_fill_gradient2()
    
}



