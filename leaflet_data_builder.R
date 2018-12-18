library(dplyr)
View(dat)

precise_accident_data <- final_data %>%
  filter(state_name == "Idaho") %>%
  filter(county_pop >= 0) %>%
  select(COUNTY_NAME, geometry, number_of_fatalities, county_pop, geometry, longitude, latitude) %>%
  mutate(geometry2 = geometry) %>%
  as_tibble()  

View(merged_data)

library(sf) 
merged_data <- merged_data %>%
  st_as_sf()