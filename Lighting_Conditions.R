library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)


dat <- read.csv("Final_Data.csv")

project_2_dat <- dat %>%
  group_by(light_condition_name, atmospheric_conditions_1_name) %>%
  summarize(total_fatalities = sum(number_of_fatalities)) 

dat_percentage <- project_2_dat %>%
  mutate(group_fatalities = sum(total_fatalities)) %>%
  mutate(total_fatalities = total_fatalities/group_fatalities)


dat_filtered <- project_2_dat %>%
  filter(light_condition_name != "Not Reported") %>%
  filter(light_condition_name != "Unknown") %>%
  filter(light_condition_name != "Other") %>%
  filter(atmospheric_conditions_1_name != "Unknown") %>%
  filter(atmospheric_conditions_1_name != "Not Reported") %>%
  filter(atmospheric_conditions_1_name != "Other")

dat_matrix_prep <- dat_filtered %>%
  mutate(total_fatalities = as.numeric(total_fatalities)) %>%
  spread(atmospheric_conditions_1_name, total_fatalities, fill=0)

dat_as_matrix <- matrix(unlist(dat_matrix_prep), 6)
# set the rownames
#dat_as_matrix <- dat_as_matrix[,-1]

# Convert to numeircm atrix
dat_as_matrix_b <- dat_as_matrix[,-1] %>%
  mapply(dat_as_matrix, FUN=as.numeric) %>%
  matrix(ncol=10, nrow=6)

rownames(dat_as_matrix_b) <- c(dat_as_matrix[1:6,1])

matrix_col_names <- c("Blowing Sand, Soil, Dirt", "Blowing Snow",
                      "Clear", "Cloudy", "Fog, Smog, Smoke",
                      "Freezing Rain or Drizzle", "Rain",
                      "Severe Crosswinds", "Sleet, Hail",
                      "Snow")

colnames(dat_as_matrix_b) <- matrix_col_names