View(final_data)

population_vs_fatalities <- final_data %>%
  filter(state_name=="Idaho") %>%
  filter(county_pop >= 0) %>%
  select(COUNTY_NAME, county_pop, number_of_fatalities) %>%
  group_by(COUNTY_NAME, county_pop) %>%
  summarize(
    total_fatalities = sum(number_of_fatalities)
  ) %>%
  mutate(county_pop = county_pop/1000)


 ggplot(
   data=population_vs_fatalities,
   aes(x=COUNTY_NAME,y=total_fatalities, size=county_pop, label=COUNTY_NAME)
 ) +
   ylab("Total Fatalities") +
   xlab("County Population in Thousands") +
   geom_text(angle= 45)
 