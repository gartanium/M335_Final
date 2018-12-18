import numpy as np 
import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt

import bq_helper
# https://www.kaggle.com/usdot/nhtsa-traffic-fatalities
# create a helper object for this dataset
accidents = bq_helper.BigQueryHelper(active_project="bigquery-public-data",
                                   dataset_name="nhtsa_traffic_fatalities")
								 
								 
query = """SELECT state_name, county, city, hour_of_crash, day_of_crash, month_of_crash,
                  light_condition_name, atmospheric_conditions_1_name,
				  number_of_fatalities, number_of_drunk_drivers, timestamp_of_crash,
				  national_highway_system, latitude, longitude
		   FROM `bigquery-public-data.nhtsa_traffic_fatalities.accident_2015`
        """

accidents = accidents.query_to_pandas(query)
accidents.to_csv("Accidents_Data.csv")