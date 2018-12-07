import numpy as np
import pandas as pd
from pandas import DataFrame
import matplotlib.pyplot as plt

# bq_helper
# pip install -e git+https://github.com/SohierDane/BigQuery_Helper#egg=bq_helper

from google.cloud import bigquery
from bq_helper import BigQueryHelper

db_traffic = BigQueryHelper("bigquery-public-data", "nhtsa_traffic_fatalities")

query = """select hour_of_crash, count(consecutive_number)count_of_crashes
           from `bigquery-public-data.nhtsa_traffic_fatalities.accident_2016`
           group by hour_of_crash
           order by 2 desc """

crashes_per_hour = db_traffic.query_to_pandas(query)

crashes_per_hour.to_csv("Crash_Data.csv")

x = crashes_per_hour['hour_of_crash']
y = crashes_per_hour['count_of_crashes']
y_pos = np.arange(len(y))

plt.bar(y_pos, y, align="center",alpha=0.5)
plt.xticks(y_pos, x)
plt.ylabel('count of crashes')
plt.title('Count of crashes per hour')
plt.show()