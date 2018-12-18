library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

display_population_fatalities <- function() {
  us_population_2010 <-  309300000
  us_average_fatalities <- 31571 * 100000/ 309000000
  
  final_data <- read.csv("Final_Data.csv")
  
  state_pop_data <- final_data %>%
    group_by(state_name)
  
  clean_data <- final_data %>%
    filter(!is.na(county_pop)) %>%
    filter(as.numeric(county_pop) > 0)
  
  # Total fatalities by Population (2010 Census)
  county_fatality_rates <- clean_data %>%
    group_by(SC_ID) %>%
    summarize(county_fatality_per_hundred_thousand = sum(number_of_fatalities) * 100000 / max(county_pop)) %>%
    ungroup() 
  
  idaho_fatality_county_rates <- clean_data %>%
    filter(state_name == "Idaho") %>%
    group_by(COUNTY_NAME) %>%
    summarize(fatality_rate = sum(number_of_fatalities) * 100000 / sum(county_pop)) %>%
    ungroup() %>%
    arrange(desc(fatality_rate)) %>%
    mutate(highlighted = (COUNTY_NAME == "Madison") | (COUNTY_NAME == "Bonneville") )
  
  idaho_fatality_county_rates$COUNTY_NAME <-
    factor(
      idaho_fatality_county_rates$COUNTY_NAME,
      levels = 
        idaho_fatality_county_rates$COUNTY_NAME[
          order(
            -idaho_fatality_county_rates$fatality_rate
          )
        ]
    )
  
  
  idaho_average_fatality_rate <- (idaho_fatality_county_rates %>%
    summarize(average_rate = mean(fatality_rate)))$average_rate[1]
  
    
  # Total rate per 100,000 people
  US_Average_Fatality_Rate_Per_hundred_thousand <- 
    county_fatality_rates %>%
    summarize(average_rate = mean(county_fatality_per_hundred_thousand))
   
  
  ################################################################
  ## BUILD THE IDAHO TOTAL FATALITIES DATA  
  idaho_total_county_fatalities <- clean_data %>%
    filter(state_name == "Idaho") %>%
    group_by(COUNTY_NAME) %>%
    summarize(total_fatalities = sum(number_of_fatalities)) %>%
    mutate(highlighted = (COUNTY_NAME == "Madison") | (COUNTY_NAME == "Bonneville") )
  
  idaho_total_county_fatalities$COUNTY_NAME <-
    factor(
      idaho_total_county_fatalities$COUNTY_NAME,
      levels = 
        idaho_total_county_fatalities$COUNTY_NAME[
          order(
            -idaho_total_county_fatalities$total_fatalities
          )
          ]
    )
  
  
  # Displays the total number of fatalities.
  ggplot() +
    geom_col(
      data=idaho_total_county_fatalities,
      aes(x=COUNTY_NAME, y=total_fatalities, fill=highlighted)
    ) +
    theme(axis.text.x = element_text(angle=45, hjust=1))  +
    scale_fill_discrete(guide = 'none') +
    geom_text(
      data=idaho_total_county_fatalities,
      aes(label = total_fatalities, y = total_fatalities + 0.1, x=COUNTY_NAME),
      position = position_dodge(0.9),
      vjust = 0
    ) +
    ylab("Total Fatalities") +
    xlab("County Name") +
    ggtitle("Total Number of Car Related Fatalities In Idaho For 2015")
  
  ################################################################
  
  
  # Displays the total fatality rates.
  ggplot() +
    geom_col(
      data=idaho_fatality_county_rates,
      aes(x=COUNTY_NAME, y=fatality_rate, fill=highlighted)
    ) +
    theme(axis.text.x = element_text(angle=45, hjust=1))  +
    geom_hline(aes(yintercept=us_average_fatalities)) +
    geom_text(
      aes(
        label="National Average Fatality Rate", 
        5, 
        us_average_fatalities, 
        vjust=-1)
    ) +
    geom_hline(aes(yintercept=idaho_average_fatality_rate)) +
    geom_text(
      aes(label="Idaho Average County Rate", 5,idaho_average_fatality_rate, vjust=-1)
      ) +
    scale_fill_discrete(guide = 'none') +
    ylab("Fatalities Per 100,000 People") +
    xlab("County Name") +
    ggtitle("Car Related Fatality Rates In Idaho For 2015") +
    geom_text(
      data=idaho_fatality_county_rates,
      aes(label = floor(fatality_rate), y = fatality_rate + 0.1, x=COUNTY_NAME),
      position = position_dodge(0.9),
      vjust = 0
    ) 
  

}

display_population_fatalities()
