library(tidyverse)
library(rworldmap)
library(dplyr)
library(raster)
df_people = read_csv("data/ru_air_people.csv")
df_cargo = read_csv("data/ru_air_cargo.csv")

regExp = "(\\d+\\.\\d+)"

#------------------------------------------------------------------------------

df_p_fresh = df_people %>%
  filter(`Whole year` > 0)

df_pass_ave = df_p_fresh %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(total_flights = sum(`Whole year`), ave_flights = round(mean(`Whole year`), digits = 0),
            jan = round(mean(January)), feb = round(mean(February)), mar = round(mean(March)), apr = round(mean(April)),
            may = round(mean(May)), jun = round(mean(June)), jul = round(mean(July)), aug = round(mean(August)), sep = round(mean(September)),
            oct = round(mean(October)), nov = round(mean(November)), dec = round(mean(December)),
            location = first(`Airport coordinates`))


strlonglat = str_extract_all(df_pass_ave$location, pattern = regExp)
#creating column lat and long
i = 1
for (e in strlonglat) {
  df_pass_ave$long[i] = as.double(e[1])
  df_pass_ave$lat[i] = as.double(e[2])
  i = i + 1
}
#Deleting airports with no lat and long
df_pass_ave = subset(df_pass_ave, long != "NA" & lat != "NA")
#------------------------------------------------------------------------------
map = getData("GADM", country = "Russia", level = 1)

#People data plot
#All airports within the location range of: longitude -> 30 - 50 ; latitude -> 40 - 60 (years )
plot(map)
pLocationRange = df_pass_ave %>% filter((30 < df_pass_ave$long & df_pass_ave$long < 50)
                                          & (40 < df_pass_ave$lat & df_pass_ave$lat < 60))
points(pLocationRange$long, pLocationRange$lat, col = "red", cex = 1)

#All airports with the lowest average flight details in Winter (December, January & February) i.e. less than 200 flights

plot(map)
pWinterLowest = df_pass_ave %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(ave_flights_winter = round(mean(dec, jan, feb), digits = 0), long = long, lat = lat) %>%
  filter(ave_flights_winter < 200)
points(pWinterLowest$long, pWinterLowest$lat, col = "red", cex = 1)
#All airports with the highest total average flight details in Summer (June, July & August) i.e. greater than 5000 flights

plot(map)
pSummerHighest = df_pass_ave %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(ave_flights_summer = round(mean(jun, jul, aug), digits = 0), long = long, lat = lat) %>%
  filter(ave_flights_summer > 5000)
points(pSummerHighest$long, pSummerHighest$lat, col = "red", cex = 1)

#All airports with the lowest total average flight details in Summer (June, July & August) i.e. less than than 5000 flights

plot(map)
pSummerLowest = df_pass_ave %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(ave_flights_summer = round(mean(jun, jul, aug), digits = 0), long = long, lat = lat) %>%
  filter(ave_flights_summer < 5000)
points(pSummerLowest$long, pSummerLowest$lat, col = "red", cex = 1)

#df_cargo
#------------------------------------------------------------------------------
df_c_fresh = df_cargo %>%
  filter(`Whole year` > 0)

df_cargo_ave = df_c_fresh %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(total_flights = sum(`Whole year`), ave_flights = round(mean(`Whole year`), digits = 0),
            jan = round(mean(January)), feb = round(mean(February)), mar = round(mean(March)), apr = round(mean(April)),
            may = round(mean(May)), jun = round(mean(June)), jul = round(mean(July)), aug = round(mean(August)), sep = round(mean(September)),
            oct = round(mean(October)), nov = round(mean(November)), dec = round(mean(December)),
            location = first(`Airport coordinates`))

strlonglat = str_extract_all(df_cargo_ave$location, pattern = regExp)
#creating column lat and long
i = 1
for (e in strlonglat) {
  df_cargo_ave$long[i] = as.double(e[1])
  df_cargo_ave$lat[i] = as.double(e[2])
  i = i + 1
}
#Deleting airports with no lat and long
df_cargo_ave = subset(df_cargo_ave, long != "NA" & lat != "NA")
#------------------------------------------------------------------------------
#Cargo data plot
#All airports within the location range of: longitude -> 30 - 50 ; latitude -> 40 - 60 (years )

plot(map)
cLocationRange = df_cargo_ave %>% filter((30 < df_cargo_ave$long & df_cargo_ave$long < 50)
                                       & (40 < df_cargo_ave$lat & df_cargo_ave$lat < 60))
points(cLocationRange$long, cLocationRange$lat, col = "red", cex = 1)

#All airports with the lowest average flight details in Winter (December, January & February) i.e. less than 200 flights

plot(map)
cWinterLowest = df_cargo_ave %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(ave_flights_winter = round(mean(dec, jan, feb), digits = 0), long = long, lat = lat) %>%
  filter(ave_flights_winter < 200)
points(cWinterLowest$long, cWinterLowest$lat, col = "red", cex = 1)

#All airports with the highest total average flight details in Summer (June, July & August) i.e. greater than 5000 flights

plot(map)
cSummerHighest = df_cargo_ave %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(ave_flights_summer = round(mean(jun, jul, aug), digits = 0), long = long, lat = lat) %>%
  filter(ave_flights_summer > 5000)
points(cSummerHighest$long, cSummerHighest$lat, col = "red", cex = 1)

#All airports with the lowest total average flight details in Summer (June, July & August) i.e. less than than 5000 flights

plot(map)
cSummerLowest = df_cargo_ave %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(ave_flights_summer = round(mean(jun, jul, aug), digits = 0), long = long, lat = lat) %>%
  filter(ave_flights_summer < 5000)
points(cSummerLowest$long, cSummerLowest$lat, col = "red", cex = 1)