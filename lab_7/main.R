library(dplyr)
library(anomalize)
library(tidyverse)
library(tibble)
library(tibbletime)
data = read.csv("data/County_time_series.csv")

# Anomalies in MedianListingPrice_AllHomes Where MedianListingPricePerSqft_AllHomes > 65
data_filtered_more_than_65 = data %>%
  filter(RegionName == 1001 | RegionName == 1003) %>%
  filter(MedianListingPricePerSqft_AllHomes > 65) %>%
  select(Date, RegionName, MedianListingPrice_AllHomes, MedianListingPricePerSqft_AllHomes) %>%
  drop_na()
t_more_than_65 = data_filtered_more_than_65 %>%
  mutate(date = as.Date(data_filtered_more_than_65$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

ts_more_than_65 = t_more_than_65 %>%
  time_decompose(MedianListingPrice_AllHomes) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "MedianListingPrice_AllHomes more than 65 Anomalies")
plot(ts_more_than_65)


# Anomalies in MedianListingPrice_AllHomes Where MedianListingPricePerSqft_AllHomes < 65
data_filtered_less_than_65 = data %>%
  filter(RegionName == 1059 | RegionName == 17137) %>%
  filter(MedianListingPricePerSqft_AllHomes < 65) %>%
  select(Date, RegionName, MedianListingPrice_AllHomes, MedianListingPricePerSqft_AllHomes) %>%
  drop_na()
t_less_than_65 = data_filtered_less_than_65 %>%
  mutate(date = as.Date(data_filtered_less_than_65$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

ts_less_than_65 = t_less_than_65 %>%
  time_decompose(MedianListingPrice_AllHomes) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "MedianListingPrice_AllHomes less than 65 Anomalies")
plot(ts_less_than_65)

# General anomaly detection in prices of Homes with more than 5 rooms ZHVI_5BedroomOrMore
data_filtered_5_bedroom = data %>%
  filter(RegionName == 1001 | RegionName == 1003) %>%
  select(Date, RegionName, ZHVI_5BedroomOrMore) %>%
  drop_na()

t_filtered_5_bedroom <- data_filtered_5_bedroom %>%
  mutate(date = as.Date(data_filtered_5_bedroom$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

ts_zhvi_5_bedroom_or_more = t_filtered_5_bedroom %>%
  time_decompose(ZHVI_5BedroomOrMore) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_5BedroomOrMore Anomalies")
plot(ts_zhvi_5_bedroom_or_more)

# General anomaly detection in prices of low cost homes ZHVI_BottomTier
data_filtered_bottom_tier = data %>%
  filter(RegionName == 1001 | RegionName == 1003) %>%
  select(Date, RegionName, ZHVI_BottomTier) %>%
  drop_na()
t_filtered_bottom_tier <- data_filtered_bottom_tier %>%
  mutate(date = as.Date(data_filtered_bottom_tier$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")
ts_zhvi_bottom_tier = t_filtered_bottom_tier %>%
  time_decompose(ZHVI_BottomTier) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_BottomTier Anomalies")
plot(ts_zhvi_bottom_tier)

# General anomaly detection in prices of 1-bedroom ZHVI_1bedroom
data_filtered_1_bedroom = data %>%
  filter(RegionName == 1001 | RegionName == 1003) %>%
  select(Date, RegionName, ZHVI_1bedroom) %>%
  drop_na()
t_filtered_1_bedroom <- data_filtered_1_bedroom %>%
  mutate(date = as.Date(data_filtered_1_bedroom$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")
ts_zhvi_1bedroom = t_filtered_1_bedroom %>%
  time_decompose(ZHVI_1bedroom) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_1bedroom Anomalies")
plot(ts_zhvi_1bedroom)

# General anomaly detection in prices of 2-bedroom ZHVI_2bedroom
data_filtered_2_bedroom = data %>%
  filter(RegionName == 1001 | RegionName == 1003) %>%
  select(Date, RegionName, ZHVI_2bedroom) %>%
  drop_na()
t_filtered_2_bedroom <- data_filtered_2_bedroom %>%
  mutate(date = as.Date(data_filtered_2_bedroom$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")
ts_zhvi_2bedroom = t_filtered_2_bedroom %>%
  time_decompose(ZHVI_2bedroom) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_2bedroom Anomalies")
plot(ts_zhvi_2bedroom)

# General anomaly detection in prices of 3-bedroom ZHVI_3bedroom
data_filtered_3_bedroom = data %>%
  filter(RegionName == 1001 | RegionName == 1003) %>%
  select(Date, RegionName, ZHVI_3bedroom) %>%
  drop_na()
t_filtered_3_bedroom <- data_filtered_3_bedroom %>%
  mutate(date = as.Date(data_filtered_3_bedroom$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")
ts_zhvi_3bedroom = t_filtered_3_bedroom %>%
  time_decompose(ZHVI_3bedroom) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_3bedroom Anomalies")
plot(ts_zhvi_3bedroom)

# General anomaly detection in Home Prices Sale_Prices
data_filtered_sale_prices = data %>%
  filter(RegionName == 6037 | RegionName == 11001) %>%
  select(Date, RegionName, Sale_Prices) %>%
  drop_na()
t_sale_prices = data_filtered_sale_prices %>%
  mutate(date = as.Date(data_filtered_sale_prices$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

ts_sale_prices = t_sale_prices %>%
  time_decompose(Sale_Prices) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "Sale_Prices Anomalies")
plot(ts_sale_prices)

# General anomaly detection in Homes with decreasing Value PctOfHomesDecreasingInValues_AllHomes
data_filtered_pct_of_homes_decreasing = data %>%
  filter(RegionName == 10001 | RegionName == 10003) %>%
  select(Date, RegionName, PctOfHomesDecreasingInValues_AllHomes) %>%
  drop_na()
t_pct_of_homes_decreasing = data_filtered_pct_of_homes_decreasing %>%
  mutate(date = as.Date(data_filtered_pct_of_homes_decreasing$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

ts_pct_of_homes_decreasing = t_pct_of_homes_decreasing %>%
  time_decompose(PctOfHomesDecreasingInValues_AllHomes) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "PctOfHomesDecreasingInValues_AllHomes Anomalies")
plot(ts_pct_of_homes_decreasing)

