library(dplyr)
library(anomalize)
library(tidyverse)
library(tibble)
library(tibbletime)
data = read.csv("data/County_time_series.csv")
data_filtered = data %>%
  filter(RegionName == 1001 | RegionName == 1003) %>%
  select(Date, RegionName, MedianListingPrice_AllHomes, MedianListingPricePerSqft_AllHomes, ZHVI_5BedroomOrMore, ZHVI_BottomTier, ZHVI_1bedroom, ZHVI_2bedroom, ZHVI_3bedroom)

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

data_with_date <- data_filtered %>%
  mutate(date = as.Date(data_filtered$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

ts_zhvi_5_bedroom_or_more = data_with_date %>%
  time_decompose(ZHVI_5BedroomOrMore) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_5BedroomOrMore Anomalies")

ts_zhvi_bottom_tier = data_with_date %>%
  time_decompose(ZHVI_BottomTier) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_BottomTier Anomalies")


ts_zhvi_1bedroom = data_with_date %>%
  time_decompose(ZHVI_1bedroom) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_1bedroom Anomalies")

ts_zhvi_2bedroom = data_with_date %>%
  time_decompose(ZHVI_2bedroom) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_2bedroom Anomalies")

ts_zhvi_3bedroom = data_with_date %>%
  time_decompose(ZHVI_3bedroom) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
  labs(title = "ZHVI_3bedroom Anomalies")

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